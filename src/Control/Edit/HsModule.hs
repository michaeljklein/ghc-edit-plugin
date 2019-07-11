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
import Data.Foldable
import FastString
import GHC
import HscTypes
import qualified Name
import SrcLoc
import Control.Monad.IO.Class
import Control.Monad
import Control.Arrow (Kleisli(..))

import Language.Haskell.TH.Syntax
import Data.List
import Data.Bifunctor
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
parseMergeWithLImportDecls :: FastString -> [String] -> EditedM [LImportDecl GhcPs] ()
parseMergeWithLImportDecls info newImportDeclStrs = EditedM . Kleisli $ \importDecls' -> EditM $ do
  newImportDecls <- liftIO $ mapM (runGHC . parseImportDecl) newImportDeclStrs
  (fmap runEditM . runKleisli . runEditedM) (mergeWithLImportDecls info newImportDecls) importDecls'

-- | Make an `Edited` that merges a list of @`ImportDecl` GhcPs`@ into another,
-- given an information string to provide to `mkGeneralSrcSpan`
mergeWithLImportDecls ::
     FastString -> [ImportDecl GhcPs] -> EditedM [LImportDecl GhcPs] ()
mergeWithLImportDecls info newImportDecls =
  void . EditedM . Kleisli $ \xs ->
    EditM $ do
      ~(addedXs, ys) <-
        fmap (bimap (fmap (L (mkGeneralSrcSpan info)) . toList) Just) $ do
          foldM
            (\ ~(newImportDeclMap', imports') import' -> do
               let importId' = getImportId $ unLocated import'
               case Map.lookup importId' newImportDeclMap' of
                 Nothing -> return $ (newImportDeclMap', imports' ++ [import'])
                 ~(Just y) ->
                   return $
                   ( Map.delete importId' newImportDeclMap'
                   , imports' ++
                     [mergeImportDecl info (unLocated import') y <$ import']))
            (newImportDeclMap, [])
            xs
      return . (,) () $ maybe (maybeNonEmpty addedXs) (Just . (++ addedXs)) ys
  where
    getImportId ::
         ImportDecl GhcPs
      -> (Bool, Maybe Bool, Maybe StringLiteral, Maybe ModuleName, ModuleName)
    getImportId =
      liftM5
        (,,,,)
        ideclQualified
        (fmap fst . ideclHiding)
        ideclPkgQual
        (fmap unLocated . ideclAs)
        (unLocated . ideclName)

    newImportDeclMap ::
         Map ( Bool
             , Maybe Bool
             , Maybe StringLiteral
             , Maybe ModuleName
             , ModuleName) (ImportDecl GhcPs)
    newImportDeclMap =
      Map.fromList $ first getImportId . join (,) <$> newImportDecls

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

-- | If for each `LHsDecl`, we can return an `EditM` list of them,
-- we can edit a `HsModule`
addLHsModDecls ::
     (LHsDecl pass -> EditM a [LHsDecl pass]) -> EditedM (HsModule pass) [a]
addLHsModDecls = (`editConcatMapAfterM` editHsModulehsmodDecls)

-- | `addLHsModDecls` where each `HsDecl` is provided with
-- a `FastString` to make a `SrcSpan` using `UnhelpfulSpan`.
addHsModDecls ::
     (HsDecl pass -> EditM a [(FastString, HsDecl pass)])
  -> EditedM (HsModule pass) [a]
addHsModDecls f =
  addLHsModDecls $ (fmap . fmap) (uncurry (L . UnhelpfulSpan)) . f . unLocated

-- | Lift `addHsModDecls` to accept `HsSplice`'s
addSplices ::
     (HsDecl GhcPs -> EditM a [(FastString, HsSplice GhcPs)])
  -> EditedM (HsModule GhcPs) [a]
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
     (TyClDecl GhcPs -> EditM () [(FastString, HsSplice GhcPs)])
  -> EditedM (HsModule GhcPs) ()
addTyClDeclSplices f =
  void . addSplices $ \hsDecl' ->
    case tyClDecl hsDecl' of
      Nothing -> EditM $ return ((), Nothing)
      ~(Just tyClDecl') -> f tyClDecl'

-- | Run `tyClDeclTypeName` on the `TyClDecl` in `addTyClDeclSplices`
addTyClDeclTypeNameSplices ::
     (TyClDeclTypeName -> Located (IdP GhcPs) -> EditM () [( FastString
                                                           , HsSplice GhcPs)])
  -> EditedM (HsModule GhcPs) ()
addTyClDeclTypeNameSplices f =
  addTyClDeclSplices $ \tyClDecl' ->
    case tyClDeclTypeName tyClDecl' of
      Nothing -> EditM $ return ((), Nothing)
      ~(Just tyClDeclTypeName') -> uncurry f tyClDeclTypeName'

-- | Make a splice for `addTyClDeclTypeNameSplices` by using `spliceApp`
-- with the given TH function names, applied to each `TyClDecl` name.
addTyClDeclTypeNameSpliceFunctions ::
     (TyClDeclTypeName -> EditM () [(FastString, IdP GhcPs)])
  -> EditedM (HsModule GhcPs) ()
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

