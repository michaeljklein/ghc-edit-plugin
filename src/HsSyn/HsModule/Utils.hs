{-# LANGUAGE RecordWildCards #-}

module HsSyn.HsModule.Utils where

import Control.Edit (unLocated)
import Data.Maybe
import GHC

-- | Convert an `IE` (import/export) to a `LIEWrappedName`.
-- (skips `IEModuleContents`)
ieWrappedExportedName :: IE pass -> Maybe (LIEWrappedName (IdP pass))
ieWrappedExportedName (IEVar       _ name') = Just name'
ieWrappedExportedName (IEThingAbs  _ name') = Just name'
ieWrappedExportedName (IEThingAll  _ name') = Just name'
ieWrappedExportedName (IEThingWith _ name' _ _ _) = Just name'
ieWrappedExportedName _ = Nothing

-- | Get all `LIEWrappedName`'s in a `Located` list of `LIE`'s
ieWrappedExportedNames :: Located [LIE pass] -> Located [LIEWrappedName (IdP pass)]
ieWrappedExportedNames = fmap . mapMaybe $ ieWrappedExportedName . unLocated

-- | Get the name in a `LIEWrappedName`
getLIEWrappedName :: LIEWrappedName name -> name
getLIEWrappedName (L _ ieWrappedName') =
  case ieWrappedName' of
    IEName (L _ name') -> name'
    IEPattern (L _ name') -> name'
    IEType (L _ name') -> name'

-- | Get all declaration names in a `Located` list of `LIE`'s
ieWrappedExportedRawNames :: Located [LIE pass] -> [IdP pass]
ieWrappedExportedRawNames xs =
  case ieWrappedExportedNames xs of
    L _ ys -> getLIEWrappedName <$> ys

-- | Get all declaration names in a `FamilyDecl`
familyDeclNames :: FamilyDecl pass -> [IdP pass]
familyDeclNames FamilyDecl {..} = [unLocated fdLName]
familyDeclNames _ = []

-- | Get all declaration names in a `TyClDecl`
tyClDeclNames :: TyClDecl pass -> [IdP pass]
tyClDeclNames FamDecl {..} = familyDeclNames tcdFam
tyClDeclNames SynDecl {..} = [unLocated tcdLName]
tyClDeclNames DataDecl {..} = [unLocated tcdLName]
tyClDeclNames ClassDecl {..} = [unLocated tcdLName]
tyClDeclNames _ = []

-- | Get all declaration names in a `PatSynBind`
patSynBindNames :: PatSynBind idL idR -> [IdP idL]
patSynBindNames PSB {..} = [unLocated psb_id]
patSynBindNames _ = []

-- Patterns (`PatBind`) are not named?
-- | Get all declaration names in a `HsBind`
hsBindNames :: HsBind pass -> [IdP pass]
hsBindNames FunBind {..} = [unLocated fun_id]
hsBindNames PatBind {..} = []
hsBindNames VarBind {..} = [var_id]
hsBindNames AbsBinds {..} = []
hsBindNames (PatSynBind _ xs) = patSynBindNames xs
hsBindNames _ = []

-- | Get all declaration names in a `ForeignDecl`
foreignDeclNames :: ForeignDecl pass -> [IdP pass]
foreignDeclNames ForeignImport {..} = [unLocated fd_name]
foreignDeclNames ForeignExport {..} = [unLocated fd_name]
foreignDeclNames _ = []

-- | Get all declaration names in a `HsDecl`
hsDeclNames :: HsDecl pass -> [IdP pass]
hsDeclNames (TyClD _ xs) = tyClDeclNames xs
hsDeclNames (InstD _ _) = []
hsDeclNames (DerivD _ _) = []
hsDeclNames (ValD _ xs) = hsBindNames xs
hsDeclNames (SigD _ _) = [] -- keeping these would duplicate names from bindings
hsDeclNames (DefD _ _) = []
hsDeclNames (ForD _ xs) = foreignDeclNames xs
hsDeclNames (WarningD _ _) = []
hsDeclNames (AnnD _ _) = []
hsDeclNames (RuleD _ _) = []
hsDeclNames (SpliceD _ _) = []
hsDeclNames (DocD _ _) = []
hsDeclNames (RoleAnnotD _ _) = []
hsDeclNames (XHsDecl _) = []

-- | Get all declaration names in a `LHsDecl`
lhsDeclNames :: LHsDecl pass -> [IdP pass]
lhsDeclNames = hsDeclNames . unLocated

-- | Get all declaration names in a `HsModule`
hsModuleExportedNames :: HsModule pass -> [IdP pass]
hsModuleExportedNames HsModule {..} =
  case hsmodExports of
    Nothing -> hsmodDecls >>= lhsDeclNames
    ~(Just hsmodExports') -> ieWrappedExportedRawNames hsmodExports'

-- | Get all declaration names in a `Located` `HsModule`
lhsModuleExportedNames :: Located (HsModule pass) -> [IdP pass]
lhsModuleExportedNames = hsModuleExportedNames . unLocated


