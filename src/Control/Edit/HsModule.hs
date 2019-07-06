{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Edit.HsModule where

import BasicTypes
import Control.Applicative
import Control.Edit
import Control.Edit.HsModule.Parse
import Control.Edit.TH
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.Class
import Data.Set (Set)
import Data.Foldable
import FastString
import FieldLabel
import GHC
import HscTypes
import qualified Name
import SrcLoc
import Control.Monad.IO.Class
import Control.Monad
import Control.Arrow
import DynFlags
import Outputable hiding (empty, (<>))
import RdrName

import Language.Haskell.TH.Syntax
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- | `HsDecl` to `TyClDecl` or `Nothing` otherwise
tyClDecl :: HsDecl p -> Maybe (TyClDecl p)
tyClDecl (TyClD _ tyClDecl') = Just tyClDecl'
tyClDecl _ = Nothing

-- | Which kind of `TyClDecl` with a name is it?
data TyClDeclTypeName
  = FamTypeName
  | SynTypeName
  | DataTypeName
  | ClassTypeName
  deriving (Eq, Ord, Show, Read, Enum)

-- | Extract the `TyClDeclTypeName` and `Located` name (@`IdP` p@) from a `TyClDecl`
tyClDeclTypeName :: TyClDecl p -> Maybe (TyClDeclTypeName, Located (IdP p))
tyClDeclTypeName FamDecl{..} = Just (FamTypeName, fdLName tcdFam)
tyClDeclTypeName SynDecl{..} = Just (SynTypeName, tcdLName)
tyClDeclTypeName DataDecl {..} = Just (DataTypeName, tcdLName)
tyClDeclTypeName ClassDecl {..} = Just (DataTypeName, tcdLName)
tyClDeclTypeName _ = Nothing

$(editRecordFields ''HsParsedModule)
$(editRecordFields ''HsModule)

-- | Given an info `FastString` for `Located`, parse a list of
-- @`ImportDecl` `GhcPs`@'s in `String` form, e.g.
-- @"import Data.Bool (bool)"@
--
-- This uses `parseImportDecl` for parsing.
--
-- The resulting import declarations are passed to `mergeWithLImportDecls`,
-- which merges one list of @`ImportDecl` `GhcPs`@' into another.
parseMergeWithLImportDecls :: FastString -> [String] -> Edited [LImportDecl GhcPs]
parseMergeWithLImportDecls info newImportDeclStrs = Edited . Kleisli $ \importDecls' -> do
  newImportDecls <- liftIO $ mapM (runGHC . parseImportDecl) newImportDeclStrs
  (runKleisli . runEdited) (mergeWithLImportDecls info newImportDecls) importDecls'

-- | Make an `Edited` that merges a list of @`ImportDecl` GhcPs`@ into another,
-- given an information string to provide to `mkGeneralSrcSpan`
mergeWithLImportDecls :: FastString -> [ImportDecl GhcPs] -> Edited [LImportDecl GhcPs]
mergeWithLImportDecls info newImportDecls = Edited . Kleisli $ \xs -> execWriterT $ do
  (>>= tell) . fmap (fmap (L (mkGeneralSrcSpan info)) . toList) . flip execStateT newImportDeclMap . forM_ xs $ \x -> do
    newImportDeclMap' <- get
    let xId = importId $ unLocated x
    case Map.lookup xId newImportDeclMap' of
      Nothing -> Control.Monad.Trans.Class.lift . tell $ [x]
      ~(Just y) -> do
        put $ Map.delete xId newImportDeclMap'
        Control.Monad.Trans.Class.lift . tell $ [mergeImportDecl info (unLocated x) y <$ x]
  where
    importId :: ImportDecl GhcPs -> (Bool, Maybe Bool, Maybe StringLiteral, Maybe ModuleName, ModuleName)
    importId = liftM5 (,,,,) ideclQualified (fmap fst . ideclHiding) ideclPkgQual (fmap unLocated . ideclAs) (unLocated . ideclName)

    newImportDeclMap :: Map (Bool, Maybe Bool, Maybe StringLiteral, Maybe ModuleName, ModuleName) (ImportDecl GhcPs)
    newImportDeclMap = Map.fromList $ first importId . join (,) <$> newImportDecls

-- | Apply `mergeLIEs` to the `ideclHiding`'s of two @`ImportDecl` `GhcPs`@'s,
-- if they're both `Just`, otherwise keep the second before the first.
mergeImportDecl :: FastString -> ImportDecl GhcPs -> ImportDecl GhcPs -> ImportDecl GhcPs
mergeImportDecl info xs ys =
  xs {
    ideclHiding = (do
      ~(hiding, xs') <- ideclHiding xs
      ~(_, ys') <- ideclHiding ys
      return (hiding, mergeLIEs info (unLocated xs') (unLocated ys') <$ xs')
      ) <|> ideclHiding ys
        <|> ideclHiding xs
  }

-- | Apply `mergeLIE` to each @`LIE` `GhcPs`@ in the first list
mergeLIEs :: FastString -> [LIE GhcPs] -> [LIE GhcPs] -> [LIE GhcPs]
mergeLIEs _ [] = id
mergeLIEs info ~(x:xs) = mergeLIEs info xs . mergeLIE info x

-- | Merge two @`LIE` `GhcPs`@'s by looking for the equivalent case,
-- overwriting it if it's just a name or something that can't be merged and
-- otherwise merging the contents.
mergeLIE :: FastString -> LIE GhcPs -> [LIE GhcPs] -> [LIE GhcPs]
mergeLIE _ x [] = [x]
mergeLIE info x ~(y:ys) =
  case unLocated x of
    IEVar       xie _ ->
      case unLocated y of
        IEVar _ lieWrappedName' -> loc (IEVar  xie lieWrappedName') : ys
        _ -> y : mergeLIE info x ys
    IEThingAbs xie _ ->
      case unLocated y of
        IEThingAbs _ lieWrappedName' -> loc (IEThingAbs xie lieWrappedName') : ys
        _ -> y : mergeLIE info x ys
    IEThingAll xie _ ->
      case unLocated y of
        IEThingAll _ lieWrappedName' -> loc (IEThingAll xie lieWrappedName') : ys
        _ -> y : mergeLIE info x ys
    IEThingWith xie _ _ names labels ->
      case unLocated y of
        IEThingWith _ lieWrappedName' wild' names' labels' ->
          loc (IEThingWith xie lieWrappedName' wild' (union names names') (union labels labels')) : ys
        _ -> y : mergeLIE info x ys
    IEModuleContents       xie _ ->
      case unLocated y of
        IEModuleContents _ lieWrappedName' -> loc (IEModuleContents xie lieWrappedName') : ys
        _ -> y : mergeLIE info x ys
    IEGroup       xie _ _ ->
      case unLocated y of
        IEGroup _ i' str' -> loc (IEGroup xie i' str') : ys
        _ -> y : mergeLIE info x ys
    IEDoc       xie _ ->
      case unLocated y of
        IEDoc _ lieWrappedName' -> loc (IEDoc xie lieWrappedName') : ys
        _ -> y : mergeLIE info x ys
    IEDocNamed       xie _ ->
      case unLocated y of
        IEDocNamed _ lieWrappedName' -> loc (IEDocNamed xie lieWrappedName') : ys
        _ -> y : mergeLIE info x ys
    XIE _ -> y:ys
  where
    loc = L $ mkGeneralSrcSpan info

deriving instance Ord SourceText
deriving instance Ord StringLiteral

-- deriving instance Ord a => Ord (IEWrappedName a)
-- deriving instance Ord IEWildcard
-- deriving instance Ord a => Ord (FieldLbl a)
-- instance Ord HsDocString where
--   compare x y = unpackHDS x `compare` unpackHDS y


-- | If for each `LHsDecl`, we can return an `EditM` list of them,
-- we can edit a `HsModule`
addLHsModDecls ::
     (LHsDecl pass -> EditM [LHsDecl pass]) -> Edited (HsModule pass)
addLHsModDecls = (`editConcatMapAfterM` editHsmodDecls)

-- | `addLHsModDecls` where each `HsDecl` is provided with
-- a `FastString` to make a `SrcSpan` using `UnhelpfulSpan`.
addHsModDecls ::
     (HsDecl pass -> EditM [(FastString, HsDecl pass)])
  -> Edited (HsModule pass)
addHsModDecls f =
  addLHsModDecls $ (fmap . fmap) (uncurry (L . UnhelpfulSpan)) . f . unLocated

-- | Lift `addHsModDecls` to accept `HsSplice`'s
addSplices ::
     (HsDecl GhcPs -> EditM [(FastString, HsSplice GhcPs)])
  -> Edited (HsModule GhcPs)
addSplices f =
  addHsModDecls $
  (fmap . fmap)
    (\ ~(info, x) ->
       ( info
       , SpliceD NoExt .
         flip (SpliceDecl NoExt) ExplicitSplice . L (UnhelpfulSpan info) $
         x)) .
  f

-- | Lift `addSplices` to accept `TyClDecl`'s and ignore other `HsDecl`'s
addTyClDeclSplices ::
     (TyClDecl GhcPs -> EditM [(FastString, HsSplice GhcPs)])
  -> Edited (HsModule GhcPs)
addTyClDeclSplices f =
  addSplices $ \hsDecl' ->
    case tyClDecl hsDecl' of
      Nothing -> empty
      ~(Just tyClDecl') -> f tyClDecl'

-- | Run `tyClDeclTypeName` on the `TyClDecl` in `addTyClDeclSplices`
addTyClDeclTypeNameSplices ::
     (TyClDeclTypeName -> Located (IdP GhcPs) -> EditM [( FastString
                                                        , HsSplice GhcPs)])
  -> Edited (HsModule GhcPs)
addTyClDeclTypeNameSplices f =
  addTyClDeclSplices $ \tyClDecl' ->
    case tyClDeclTypeName tyClDecl' of
      Nothing -> empty
      ~(Just tyClDeclTypeName') -> uncurry f tyClDeclTypeName'

-- | Make a splice for `addTyClDeclTypeNameSplices` by using `spliceApp`
-- with the given TH function names, applied to each `TyClDecl` name.
addTyClDeclTypeNameSpliceFunctions ::
     (TyClDeclTypeName -> EditM [(FastString, IdP GhcPs)])
  -> Edited (HsModule GhcPs)
addTyClDeclTypeNameSpliceFunctions f =
  addTyClDeclTypeNameSplices $ \tyClDeclTypeName' idP' ->
    fmap (\ ~(infoStr, fId) -> (infoStr, spliceApp infoStr fId (unLocated idP'))) <$>
    f tyClDeclTypeName'

-- | Given an info/debug `FastString`, a function name, and an argument name,
-- return a `HsSplice` where the function is applied to the argument.
spliceApp :: FastString -> IdP GhcPs -> IdP GhcPs -> HsSplice GhcPs
spliceApp infoStr fId xId = HsUntypedSplice NoExt HasParens spliceId $ L info spliceExpr'
  where
    info :: SrcSpan
    info = mkGeneralSrcSpan infoStr

    spliceId :: IdP GhcPs
    spliceId = Unqual $ Name.mkOccName Name.varName "splice"

    spliceExpr' :: HsExpr GhcPs
    spliceExpr' = HsApp NoExt  fs xs

    fs :: LHsExpr GhcPs
    fs = L info . HsVar NoExt $ L info fId

    xs :: LHsExpr GhcPs
    xs = L info . HsBracket NoExt $ VarBr NoExt False xId



-- foo :: DynFlags -> [LHsDecl GhcPs] -> String
-- foo fs = unlines . fmap (foos fs . unLocated)

-- foos :: DynFlags -> HsDecl GhcPs -> String
-- foos fg (SpliceD _ xs@(SpliceDecl _ (L _ ys@(HsUntypedSplice _ sd (Unqual idP') (L _ zs@(HsApp _ fs@(L _ (HsVar _ _)) args@(L _ (HsBracket _ (VarBr _ b _))))))) sef)) =
--   unlines
--     (("  " ++) <$>
--      [ "HsUntypedSplice : "
--      -- , show $ occNameSpace idP'
--      , showSDoc fg $ pprNameSpace $ occNameSpace idP'
--      , showSDoc fg (ppr (fs, args))
--      , show (b, sd)
--      , showSDoc fg (ppr idP')
--      , showSDoc fg (ppr zs)
--      , showSDoc fg (ppr ys)
--      , show sef
--      , showSDoc fg (ppr xs)
--      ]) ++
--   "\n"
-- foos fg (SpliceD _ xs@(SpliceDecl _ (L _ ys@(HsUntypedSplice _ sd idP' zs)) sef)) =
--   unlines
--     (("  " ++) <$>
--      [ "HsUntypedSplice: "
--      , show sd
--      , showSDoc fg (ppr idP')
--      , showSDoc fg (ppr zs)
--      , showSDoc fg (ppr ys)
--      , show sef
--      , showSDoc fg (ppr xs)
--      ]) ++
--   "\n"

-- foos fg (SpliceD _ xs@(SpliceDecl _ ys sef)) = unwords ["Splice: ", showSDoc fg (ppr ys), show sef, showSDoc fg (ppr xs)]
-- foos fg xs@(SpliceD _ _) = "XSpliceDecl: " ++ showSDoc fg (ppr xs)
-- foos fg xs = "NonSplice: " ++ showSDoc fg (ppr xs)

-- deriving instance Show SpliceExplicitFlag
-- -- deriving instance Show SpliceDecoration

-- -- instance Show a => Show (Bag a) where
-- --   show = show . bagToList

-- instance Show OccName where
--   show = showSDocUnsafe . pprOccName

-- deriving instance Show Module


-- -- deriving instance Show (HsBind GhcPs)
-- -- deriving instance Show (XHsWC a (LHsType a))
-- -- deriving instance Show ThBindEnv
-- deriving instance (Show a, Show b) => Show (GenLocated a b)
-- deriving instance Show (ABExport GhcPs)
-- deriving instance Show (AmbiguousFieldOcc GhcPs)
-- deriving instance Show (AnnDecl GhcPs)
-- deriving instance Show (AnnProvenance RdrName)
-- deriving instance Show (ApplicativeArg GhcPs)
-- deriving instance Show (ArithSeqInfo GhcPs)
-- deriving instance Show (Bind CoreBndr)
-- deriving instance Show (ClsInstDecl GhcPs)
-- deriving instance Show (ConDecl GhcPs)
-- deriving instance Show (ConDeclField GhcPs)
-- deriving instance Show (DataFamInstDecl GhcPs)
-- deriving instance Show (DefaultDecl GhcPs)
-- deriving instance Show (DerivDecl GhcPs)
-- deriving instance Show (DerivStrategy GhcPs)
-- deriving instance Show (FamEqn GhcPs (HsTyPats GhcPs) (HsDataDefn GhcPs))
-- deriving instance Show (FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs))
-- deriving instance Show (FamEqn GhcPs (LHsQTyVars GhcPs) (LHsType GhcPs))
-- deriving instance Show (FamInstEqn GhcPs (HsDataDefn GhcPs))
-- deriving instance Show (FamilyDecl GhcPs)
-- deriving instance Show (FamilyInfo GhcPs)
-- deriving instance Show (FamilyResultSig GhcPs)
-- deriving instance Show (FieldOcc GhcPs)
-- deriving instance Show (FixitySig GhcPs)
-- deriving instance Show (ForeignDecl GhcPs)
-- deriving instance Show (GRHS GhcPs (LHsExpr GhcPs))
-- deriving instance Show (GRHS GhcRn (LHsExpr GhcRn))
-- deriving instance Show (GRHS GhcTcId (LHsExpr GhcTcId))
-- deriving instance Show (GRHSs GhcPs (LHsExpr GhcPs))
-- deriving instance Show (GRHSs GhcRn (LHsExpr GhcRn))
-- deriving instance Show (GRHSs GhcTcId (LHsExpr GhcTcId))
-- deriving instance Show (GhcPass p)
-- deriving instance Show (HsArg (LHsType GhcPs) (LHsKind GhcPs))
-- deriving instance Show (HsBracket GhcPs)
-- deriving instance Show (HsBracket GhcRn)
-- deriving instance Show (HsConDeclDetails GhcPs)
-- deriving instance Show (HsConPatDetails GhcPs)
-- deriving instance Show (HsDataDefn GhcPs)
-- deriving instance Show (HsDecl GhcPs)
-- deriving instance Show (HsDerivingClause GhcPs)
-- deriving instance Show (HsExpr GhcPs)
-- deriving instance Show (HsExpr GhcRn)
-- deriving instance Show (HsExpr GhcTcId)
-- deriving instance Show (HsGroup GhcPs)
-- deriving instance Show (HsIPBinds GhcPs)
-- deriving instance Show (HsIPBinds GhcRn)
-- deriving instance Show (HsIPBinds GhcTcId)
-- deriving instance Show (HsImplicitBndrs GhcPs (FamEqn GhcPs (HsTyPats GhcPs) (LHsType GhcPs)))
-- deriving instance Show (HsLit GhcPs)
-- deriving instance Show (HsLocalBindsLR GhcPs GhcPs)
-- deriving instance Show (HsLocalBindsLR GhcRn GhcRn)
-- deriving instance Show (HsLocalBindsLR GhcTcId GhcTcId)
-- deriving instance Show (HsMatchContext Name)
-- deriving instance Show (HsMatchContext RdrName)
-- deriving instance Show (HsOverLit GhcPs)
-- deriving instance Show (HsPatSynDetails (Located RdrName))
-- deriving instance Show (HsPatSynDir GhcPs)
-- deriving instance Show (HsRecField' (AmbiguousFieldOcc GhcPs) (LHsExpr GhcPs))
-- deriving instance Show (HsRecField' (AmbiguousFieldOcc GhcRn) (LHsExpr GhcRn))
-- deriving instance Show (HsRecField' (FieldOcc GhcPs) (LHsExpr GhcPs))
-- deriving instance Show (HsRecField' (FieldOcc GhcPs) (LPat GhcPs))
-- deriving instance Show (HsRecField' (FieldOcc GhcRn) (LHsExpr GhcRn))
-- deriving instance Show (HsRecFields GhcPs (LPat GhcPs))
-- deriving instance Show (HsRecordBinds GhcPs)
-- deriving instance Show (HsRecordBinds GhcRn)
-- deriving instance Show (HsRecordBinds GhcTcId)
-- deriving instance Show (HsSplice GhcPs)
-- deriving instance Show (HsSplicedThing GhcPs)
-- deriving instance Show (HsStmtContext Name)
-- deriving instance Show (HsStmtContext RdrName)
-- deriving instance Show (HsTupArg GhcPs)
-- deriving instance Show (HsTyVarBndr GhcPs)
-- deriving instance Show (HsType GhcPs)
-- deriving instance Show (HsValBindsLR GhcPs GhcPs)
-- deriving instance Show (HsValBindsLR GhcRn GhcRn)
-- deriving instance Show (HsValBindsLR GhcTcId GhcTcId)
-- deriving instance Show (IPBind GhcPs)
-- deriving instance Show (IPBind GhcRn)
-- deriving instance Show (InjectivityAnn GhcPs)
-- deriving instance Show (InstDecl GhcPs)
-- deriving instance Show (LHsQTyVars GhcPs)
-- deriving instance Show (LHsSigType GhcPs)
-- deriving instance Show (LHsSigType GhcRn)
-- deriving instance Show (LHsSigWcType GhcPs)
-- deriving instance Show (LHsSigWcType GhcRn)
-- deriving instance Show (Match GhcPs (LHsExpr GhcPs))
-- deriving instance Show (Match GhcRn (LHsExpr GhcRn))
-- deriving instance Show (Match GhcTcId (LHsExpr GhcTcId))
-- deriving instance Show (MatchGroup GhcPs (LHsExpr GhcPs))
-- deriving instance Show (MatchGroup GhcRn (LHsExpr GhcRn))
-- deriving instance Show (MatchGroup GhcTcId (LHsExpr GhcTcId))
-- deriving instance Show (ParStmtBlock GhcPs GhcPs)
-- deriving instance Show (ParStmtBlock GhcRn GhcRn)
-- deriving instance Show (ParStmtBlock GhcTcId GhcTcId)
-- deriving instance Show (Pat GhcPs)
-- deriving instance Show (Pat GhcRn)
-- deriving instance Show (PatSynBind GhcPs GhcPs)
-- deriving instance Show (RecordPatSynField (Located RdrName))
-- deriving instance Show (RoleAnnotDecl GhcPs)
-- deriving instance Show (RuleBndr GhcPs)
-- deriving instance Show (RuleDecl GhcPs)
-- deriving instance Show (RuleDecls GhcPs)
-- deriving instance Show (Sig GhcPs)
-- deriving instance Show (Sig GhcRn)
-- deriving instance Show (Sig GhcTcId)
-- deriving instance Show (SpliceDecl GhcPs)
-- deriving instance Show (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
-- deriving instance Show (StmtLR GhcRn GhcRn (LHsExpr GhcRn))
-- deriving instance Show (StmtLR GhcTcId GhcTcId (LHsExpr GhcTcId))
-- deriving instance Show (SyntaxExpr GhcPs)
-- deriving instance Show (SyntaxExpr GhcRn)
-- deriving instance Show (SyntaxExpr GhcTcId)
-- deriving instance Show (Tickish Id)
-- deriving instance Show (TyClDecl GhcPs)
-- deriving instance Show (TyFamInstDecl GhcPs)
-- deriving instance Show (WarnDecl GhcPs)
-- deriving instance Show (WarnDecls GhcPs)
-- deriving instance Show Activation
-- deriving instance Show AltCon
-- deriving instance Show Boxity
-- deriving instance Show CCFlavour
-- deriving instance Show CCallConv
-- deriving instance Show CCallTarget
-- deriving instance Show CExportSpec
-- deriving instance Show CImportSpec
-- deriving instance Show CType
-- deriving instance Show ConLike
-- deriving instance Show CostCentre
-- deriving instance Show DelayedSplice
-- deriving instance Show DocDecl
-- deriving instance Show EvBind
-- deriving instance Show EvBindsVar
-- deriving instance Show EvExpr
-- deriving instance Show EvTerm
-- deriving instance Show EvTypeable
-- deriving instance Show Fixity
-- deriving instance Show FixityDirection
-- deriving instance Show ForeignExport
-- deriving instance Show ForeignImport
-- deriving instance Show Header
-- deriving instance Show HsIPName
-- deriving instance Show HsSrcBang
-- deriving instance Show HsTupleSort
-- deriving instance Show HsTyLit
-- deriving instance Show HsWrapper
-- deriving instance Show InlinePragma
-- deriving instance Show LexicalFixity
-- deriving instance Show LitNumType
-- deriving instance Show Literal
-- deriving instance Show MatchGroupTc
-- deriving instance Show Module
-- deriving instance Show NewHsTypeX
-- deriving instance Show NewOrData
-- deriving instance Show NoExt
-- deriving instance Show Origin
-- deriving instance Show OverLitVal
-- deriving instance Show OverlapMode
-- deriving instance Show PromotionFlag
-- deriving instance Show RdrName
-- deriving instance Show RecFlag
-- deriving instance Show Role
-- deriving instance Show SpliceExplicitFlag
-- deriving instance Show SrcStrictness
-- deriving instance Show SrcUnpackedness
-- deriving instance Show StringLiteral
-- deriving instance Show TcEvBinds
-- deriving instance Show TcLclEnv
-- deriving instance Show TcSpecPrag
-- deriving instance Show TcSpecPrags
-- deriving instance Show TransForm
-- deriving instance Show WarningTxt
-- deriving instance Show a => Show (BooleanFormula a)
-- deriving instance (Show a, Show (XXHsWildCardBndrs a (LHsType a)), Show (HsType a), Show (XHsWC a (LHsType a))) => Show (LHsWcType a)
-- deriving instance Show a => Show (NHsValBindsLR a)

-- instance Show DataCon where
--   show = showSDocUnsafe . ppr

-- instance Show TyCon where
--   show _ = "TyCon"
-- instance Show (a -> b) where
--   show _ = "(function)"
-- instance Show PatSyn where
--   show _ = "PatSyn"

-- instance Show (GHC.IORef.IORef a) where
--   show _ = "IORef"
-- instance Show ThModFinalizers where
--   show _ = "ThModFinalizers"
-- instance Show (HsBindLR a a) where
--   show _ = "HsBindLR a a"

-- instance Show TcCoercionR where
--   show _ = "TcCoercionR"
-- instance Show CostCentreIndex where
--   show _ = "CostCentreIndex"
-- instance (Show SDoc) where
--   show = showSDocUnsafe


-- instance Show Var where
--   show = showSDocUnsafe . ppr

-- -- instance Show Id where
-- --   show = showSDocUnsafe . ppr

-- instance Show Type where
--   show = showSDocUnsafe . ppr

-- instance Show Name where
--   show = nameStableString

--     -- • Could not deduce (Show (XHsWC a (LHsType a)))
--     -- • Could not deduce (Show (HsBindLR a a))


-- instance Show ModuleName where
--   show = moduleNameString



-- addToTyClDecls :: (TyClDecl pass -> EditM [LH

-- eachHsDecl :: (HsDecl p -> ReaderT _ Hsc [HsDecl p]) -> Edited (HsModule p)
-- eachHsDecl f = _

-- eachHsDeclPlugin :: (HsDecl GhcPs -> ReaderT _ Hsc [HsDecl GhcPs]) -> Plugin

-- (HsDecl p -> Hsc [IdP p]) -> HsDecl p -> _ [HsDecl p]

-- TODO:
-- - provide method that takes decl name -> given TyClDeclType, give (name of) TH methods to run on it
--   * it returns the resulting TH splices as HsDecl's
-- - the eachHsDecl method runs the metho on each HsDecl and then adds the resulting HsDecl's after it
-- - eachHsDeclPlugin turns it into a Plugin, and admittedly may need to also enable TH as a language pragma

-- At first, provide one that can work with different TyClDeclType's, etc. then specialize to FamType's and test with typeFamilyD

-- Data.SOP.TyFam.typeFamilyD :: Name -> Q [Dec]

-- $(Data.SOP.TyFam.typeFamilyD ''Foo)

-- idpToStringExpr :: (HasDynFlags m, Monad m) => IdP GhcPs -> m (HsExpr GhcPs)
-- idpToStringExpr x = do
--   flags <- getDynFlags
--   return .
--     HsLit NoExt . HsString NoSourceText . mkFastString . showSDoc flags . ppr $
--     x

-- HsUntypedSplice (XUntypedSplice id) SpliceDecoration (IdP id) (LHsExpr id)

--   HsUntypedSplice NoExt HasParens idP'

-- lhsExportedNameList ::
--      (HasDynFlags m, Monad m) => Located (HsModule GhcPs) -> m (LHsDecl GhcPs)
-- lhsExportedNameList lhsModule = do
--   flags <- getDynFlags
--   return .
--     ValD NoExt .
--     VarBind NoExt (mkRdrUnqual (mkOccName varName "exports")) .
--     ExplicitList NoExt Nothing $
--     idpToStringExpr flags <$> lhsModuleExportedNames lhsModule













-- editImportDeclName :: Located ModuleName >- ImportDecl p
-- editImportDeclName f ImportDecl{..} = MaybeT $ do
--   mideclName <- runMaybeT $ f ideclName
--   return $ do
--     ideclName' <- mideclName
--     return $ ImportDecl{ideclName = ideclName',..}

-- editHsModuleImports :: [LImportDecl p] >- HsModule p
-- editHsModuleImports f HsModule{..} = MaybeT $ do
--   mhsmodImports <- runMaybeT $ f hsmodImports
--   return $ do
--     hsmodImports' <- mhsmodImports
--     return $ HsModule{hsmodImports = hsmodImports',..}

-- editHsModuleImportNames :: Located ModuleName >- HsModule p
-- editHsModuleImportNames = editHsModuleImports . editList (editLocated editImportDeclName)

-- editHsModuleName :: (p ~ GhcPass pass, OutputableBndrId p) => Maybe (Located ModuleName) >- HsModule p
-- editHsModuleName f HsModule{..} = MaybeT $ do
--   mhsmodName <- runMaybeT $ f hsmodName
--   -- flags <- ask
--   -- trace ("DEBUG: " ++ (showSDoc flags . ppr) (hsmodName, mhsmodName)) $ return ()
--   -- let rr = (do
--   --   hsmodName' <- mhsmodName
--   --   return $ HsModule{hsmodName = hsmodName',..})
--   let rr = (mhsmodName >>= \hsmodName' -> return $ HsModule{hsmodName = hsmodName',..})
--   -- trace ("DEBUG2: " ++ (showSDoc flags . ppr) rr) $ return ()
--   return rr

-- editLhsModuleParsed :: Located (HsModule GhcPs) >- HsParsedModule
-- editLhsModuleParsed f HsParsedModule{..} = MaybeT $ do
--   mhpm_module <- runMaybeT $ f hpm_module
--   return $ do
--     hpm_module' <- mhpm_module
--     return $ HsParsedModule{hpm_module = hpm_module', ..}

-- editModuleName :: String -> Edited (Maybe (Located ModuleName))
-- editModuleName expectedModuleName =
--   Edited . Kleisli $ \mModuleName -> MaybeT $
--     return $
--     case mModuleName of
--       Nothing ->
--         Just . Just . L (UnhelpfulSpan $ mkFastString "editModuleName") $
--         mkModuleName expectedModuleName
--       ~(Just moduleName') ->
--         if moduleNameString (unLocated moduleName') == expectedModuleName
--           then Nothing -- traceShow (moduleNameString (unLocated moduleName'), expectedModuleName) Nothing
--           else Just . Just $ mkModuleName expectedModuleName <$ moduleName'

-- editImportName :: FilePath -> FilePath -> Edited ModuleName
-- editImportName modulePath srcPath =
--   Edited . Kleisli $ \moduleName' ->
--     MaybeT . ReaderT $ \flags -> do
--       mFoundModulePath <- liftIO $ findImportedLocalModule flags modulePath moduleName'
--       case mFoundModulePath of
--         Nothing -> return Nothing
--         ~(Just foundModulePath) -> do
--           let expectedModuleName = moduleFromPath foundModulePath srcPath
--           return $
--             if moduleNameString moduleName' == expectedModuleName
--               then Nothing
--               else Just $ mkModuleName expectedModuleName

-- -- | http://hackage.haskell.org/package/ghc-8.6.5/docs/src/Finder.html#mkHomeInstalledModule
-- mkHomeInstalledModule :: DynFlags -> ModuleName -> InstalledModule
-- mkHomeInstalledModule dflags mod_name =
--   let iuid = fst (splitUnitIdInsts (thisPackage dflags))
--   in InstalledModule iuid mod_name

-- -- | http://hackage.haskell.org/package/ghc-8.6.5/docs/src/Finder.html
-- type FileExt = String   -- Filename extension

-- -- | http://hackage.haskell.org/package/ghc-8.6.5/docs/src/Finder.html
-- type BaseName = String  -- Basename of file

-- -- | http://hackage.haskell.org/package/ghc-8.6.5/docs/src/Finder.html
-- mkHomeModLocationSearched :: DynFlags -> ModuleName -> FileExt
--                           -> FilePath -> BaseName -> IO ModLocation
-- mkHomeModLocationSearched dflags mod suff path basename = do
--    mkHomeModLocation2 dflags mod (path </> basename) suff

-- -- -----------------------------------------------------------------------------
-- -- General path searching
-- searchPathExts
--   :: [FilePath]         -- paths to search
--   -> InstalledModule             -- module name
--   -> [ (
--         FileExt,                                -- suffix
--         FilePath -> BaseName -> IO ModLocation  -- action
--        )
--      ]
--   -> IO InstalledFindResult
-- searchPathExts paths mod exts
--    = do result <- search to_search
--         return result
--   where
--     basename = moduleNameSlashes (installedModuleName mod)

--     to_search :: [(FilePath, IO ModLocation)]
--     to_search = [ (file, fn path basename)
--                 | path <- paths,
--                   (ext,fn) <- exts,
--                   let base | path == "." = basename
--                            | otherwise   = path </> basename
--                       file = base <.> ext
--                 ]

--     search [] = return (InstalledNotFound (map fst to_search) (Just (installedModuleUnitId mod)))
--     search ((file, mk_result) : rest) = do
--       b <- doesFileExist file
--       if b
--         then do { loc <- mk_result; return (InstalledFound loc mod) }
--         else search rest

-- -- | Warning: Fragile
-- findImportedLocalModule ::
--      DynFlags -> FilePath -> ModuleName -> IO (Maybe FilePath)
-- findImportedLocalModule dflags modulePath mod_name = do
--   packageDir <-
--     fromMaybe
--       (error . concat $ ["Unable to find parent package dir of: ", modulePath]) <$>
--     findParentPackageDir (takeDirectory modulePath)
--   home_path <- subdirs packageDir
--   let mod = mkHomeInstalledModule dflags mod_name
--       source_exts =
--         [ ("hs", mkHomeModLocationSearched dflags mod_name "hs")
--         , ("hs-boot", mkHomeModLocationSearched dflags mod_name "hs-boot")
--         , ("hsc", mkHomeModLocationSearched dflags mod_name "hsc")
--         , ("lhs", mkHomeModLocationSearched dflags mod_name "lhs")
--         , ("hsig", mkHomeModLocationSearched dflags mod_name "hsig")
--         , ("lhsig", mkHomeModLocationSearched dflags mod_name "lhsig")
--         ]
--   -- special case for GHC.Prim; we won't find it in the filesystem.
--   -- This is important only when compiling the base package (where GHC.Prim
--   -- is a home module).
--   if mod `installedModuleEq` gHC_PRIM
--     then return Nothing -- InstalledFound (error "GHC.Prim ModLocation") mod)
--     else do
--       xs <- searchPathExts home_path mod source_exts
--       case xs of
--         InstalledFound (ModLocation{..}) _ -> return ml_hs_file
--         _ -> return Nothing


-- -- | `editImportName` wrapped with `editedLocated`
-- editLImportName :: FilePath -> FilePath -> Edited (Located ModuleName)
-- editLImportName = fmap editedLocated . editImportName

-- updateHsModuleNames :: (p ~ GhcPass pass, OutputableBndrId p) => FilePath -> String -> FilePath -> Edited (HsModule p)
-- updateHsModuleNames srcPath expectedModuleName modulePath =
--   (editHsModuleName `runEdit` editModuleName expectedModuleName) <>
--   (editHsModuleImportNames `runEdit` editLImportName modulePath srcPath)

-- updateJustHsModuleName :: (p ~ GhcPass pass, OutputableBndrId p) => String -> Edited (HsModule p)
-- updateJustHsModuleName expectedModuleName =
--   editHsModuleName `runEdit` editModuleName expectedModuleName





-- | Parse an import declaration, e.g.
-- @$(`parseImportDeclE` "import Data.Maybe (mapMaybe)") :: `ImportDecl` `GhcPs`@
--
-- See `parseImportDecl`
-- parseImportDeclE :: String -> Q Exp
-- parseImportDeclE = (>>= lift) . liftIO . runGHC . parseImportDecl

-- deriving instance Lift (ImportDecl GhcPs)
-- deriving instance (Lift a, Lift b) => Lift (GenLocated a b)
-- deriving instance Lift RdrName
-- deriving instance Lift (IE GhcPs)
-- deriving instance Lift NoExt
-- deriving instance Lift SourceText
-- deriving instance Lift a => Lift (IEWrappedName a)
-- deriving instance Lift a => Lift (FieldLbl a)
-- deriving instance Lift SrcSpan
-- deriving instance Lift IEWildcard
-- deriving instance Lift StringLiteral

-- instance Lift HsDocString where
-- instance Lift FastString where

-- instance Lift Name.Name where
-- instance Lift GHC.Module where
-- instance Lift Name.OccName where

-- instance Lift RealSrcSpan where
--   lift = _

-- instance Lift ModuleName where
--   lift = _

-- parseImportDeclE :: String -> Q Exp
-- parseImportDeclE = (>>= lift) . liftIO . runGHC . parseImportDecl

