{-# LANGUAGE TupleSections #-}

module Control.Edit.Plugin.Echo where

import Control.Edit
import Control.Edit.HsModule
import Control.Edit.Plugin
import GhcPlugins

-- | Echo each type declaration's `TyClDeclTypeName` and name
plugin :: Plugin
plugin =
  editToPlugin $ \_ _ ->
    editHsParsedModulehpm_module `runEdit`
    editedLocated
      (addTyClDeclTypeNameSplices
         (curry $
          EditM .
          liftIO .
          fmap (, Nothing) .
          putStrLn .
          ("Control.Edit.Plugin.Echo: " ++) . show . fmap (showSDocUnsafe . ppr)))

