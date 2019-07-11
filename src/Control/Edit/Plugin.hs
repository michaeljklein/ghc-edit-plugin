{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Edit.Plugin where

import Control.Arrow
import Control.Edit
import Control.Edit.HsModule
import Control.Monad.Trans.Reader
import Data.Maybe
import FastString
import GhcPlugins hiding ((<>))
import HsExtension


-- | Convert an `Edited` to an `impurePlugin`
editToPlugin :: ([CommandLineOption] -> ModSummary -> EditedM HsParsedModule a) -> Plugin
editToPlugin editedHsModule =
  defaultPlugin {
    pluginRecompile = impurePlugin
  , parsedResultAction = editToParsedResultAction editedHsModule
  }

-- | Ignore @hs-boot@ and @hs-sig@ files
ignoringHsBootOrSig ::
     Monoid a
  => ([CommandLineOption] -> ModSummary -> EditedM HsParsedModule a)
  -> [CommandLineOption]
  -> ModSummary
  -> EditedM HsParsedModule a
ignoringHsBootOrSig f cmdOpts' modSummary'@ModSummary {..} =
  if isHsBootOrSig ms_hsc_src
    then mempty
    else f cmdOpts' modSummary'

-- | Convert an `Edited` to a `parsedResultAction`
editToParsedResultAction ::
     ([CommandLineOption] -> ModSummary -> EditedM HsParsedModule a)
  -> [CommandLineOption]
  -> ModSummary
  -> HsParsedModule
  -> Hsc HsParsedModule
editToParsedResultAction editHsParsedModule cmdOptions' modSummary'@ModSummary {..} hsParsedModule'@HsParsedModule {..} = do
  flip
    runReaderT
    (EditCxt cmdOptions' modSummary' hpm_src_files hpm_annotations) .
    fmap (fromMaybe hsParsedModule' . snd) . runEditM $
    ((runKleisli . runEditedM $ editHsParsedModule cmdOptions' modSummary')
       hsParsedModule')

-- | Given a `TyClDeclTypeName`, return an info `FastString` and the `RdrName`
-- for each TH function you'd like to apply to it
tyClDeclTypeNameSplicesPlugin ::
     (TyClDeclTypeName -> EditM () [(FastString, RdrName)])
  -> Plugin
tyClDeclTypeNameSplicesPlugin f =
  editToPlugin . ignoringHsBootOrSig $ \_ _ ->
    editHsParsedModulehpm_module `runEdit`
    editedLocated (addTyClDeclTypeNameSpliceFunctions f)

-- | `tyClDeclTypeNameSplicesPlugin` where you may provide a list of imports
-- as well as an informational `FastString`: see `parseMergeWithLImportDecls`
tyClDeclTypeNameSpliceWithImports ::
     FastString
  -> (TyClDeclTypeName -> EditM () [(FastString, IdP GhcPs)])
  -> [String]
  -> Plugin
tyClDeclTypeNameSpliceWithImports info spliceFunc importStrs =
  editToPlugin . ignoringHsBootOrSig $ \_ _ ->
    runEdit editHsParsedModulehpm_module . editedLocated $
    addTyClDeclTypeNameSpliceFunctions spliceFunc <> editHsModulehsmodImports `runEdit`
    parseMergeWithLImportDecls info importStrs

