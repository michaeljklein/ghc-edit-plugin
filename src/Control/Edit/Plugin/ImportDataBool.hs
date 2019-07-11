module Control.Edit.Plugin.ImportDataBool where

import Control.Edit
import Control.Edit.HsModule
import Control.Edit.Plugin
import FastString
import GhcPlugins

plugin :: Plugin
plugin =
  editToPlugin $ \_ _ ->
    (editHsParsedModulehpm_module) `runEdit`
    (editedLocated $
     editHsModulehsmodImports `runEdit`
     parseMergeWithLImportDecls info importDecls')
  where
    info :: FastString
    info = mkFastString "some info"

    importDecls' :: [String]
    importDecls' = ["import Data.Bool"]

