{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Edit.Plugin where

import Control.Arrow
import Control.Edit
import Control.Edit.HsModule
import Control.Monad.Trans.Maybe
import Data.Maybe
import FastString
import GhcPlugins hiding ((<>))
import HsExtension

-- | Convert an `Edited` to an `impurePlugin`
editToPlugin :: ([CommandLineOption] -> ModSummary -> Edited HsParsedModule) -> Plugin
editToPlugin editedHsModule =
  defaultPlugin {
    pluginRecompile = impurePlugin
  , parsedResultAction = editToParsedResultAction editedHsModule
  }

-- | Convert an `Edited` to a `parsedResultAction`
editToParsedResultAction ::
     ([CommandLineOption] -> ModSummary -> Edited HsParsedModule)
  -> [CommandLineOption]
  -> ModSummary
  -> HsParsedModule
  -> Hsc HsParsedModule
editToParsedResultAction editHsParsedModule cmdOptions' modSummary'@ModSummary {..} hsParsedModule'
 = do
  fmap (fromMaybe hsParsedModule') . runMaybeT $
    ((runKleisli . runEdited $ editHsParsedModule cmdOptions' modSummary')
       hsParsedModule')

-- | Given a `TyClDeclTypeName`, return an info `FastString` and the `RdrName`
-- for each TH function you'd like to apply to it
tyClDeclTypeNameSplicesPlugin ::
     (TyClDeclTypeName -> EditM [(FastString, RdrName)])
  -> Plugin
tyClDeclTypeNameSplicesPlugin f =
  editToPlugin $ \_ _ ->
    editHsParsedModulehpm_module `runEdit`
    editedLocated (addTyClDeclTypeNameSpliceFunctions f)

tyClDeclTypeNameSpliceWithImports ::
     FastString
  -> (TyClDeclTypeName -> EditM [(FastString, IdP GhcPs)])
  -> [String]
  -> Plugin
tyClDeclTypeNameSpliceWithImports info spliceFunc importStrs =
  editToPlugin $ \_ _ ->
    runEdit editHsParsedModulehpm_module . editedLocated $
    addTyClDeclTypeNameSpliceFunctions spliceFunc <> editHsModulehsmodImports `runEdit`
    parseMergeWithLImportDecls info importStrs

