-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Edit.Plugin.TyDeclNames where

import Control.Edit
import Control.Edit.Plugin
import Data.Char
import GhcPlugins hiding (Name, OccName, NameSpace)
import Language.Haskell.TH.Syntax

deriving instance Lift Name
deriving instance Lift NameFlavour
deriving instance Lift OccName
deriving instance Lift ModName
deriving instance Lift NameSpace
deriving instance Lift PkgName

-- | Make a declaration containing the given `Name` as a Haskell value,
-- named: @[`toLowerHead` (given name)]Name@
mkNameDecl :: Name -> Q [Dec]
mkNameDecl name = do
  let nameDeclName = mkName $ toLowerHead (rawName name) ++ "Name"
  nameE <- lift name
  return [ValD (VarP nameDeclName) (NormalB nameE) []]

-- | Remove all qualification from a name,
-- by returning the `last` piece returned
-- by running `dotPieces` on the `show`ed
-- `Name`
rawName :: Name -> String
rawName = last . dotPieces . show

-- | Like `words` or `lines`, but for @'.'@
-- instead of spaces or newlines
{-# NOINLINE [1] dotPieces #-}
dotPieces   :: String -> [String]
dotPieces s =  case dropWhile (== '.') s of
                [] -> []
                s' -> let (w, s'') = break (== '.') s'
                      in w : dotPieces s''

-- | Apply `toLower` to the first `Char` in a `String`, if it exists
toLowerHead :: String -> String
toLowerHead [] = []
toLowerHead ~(x:xs) = toLower x : xs

-- | Run `mkNameDecl` on all `TyClDecl`'s
plugin :: Plugin
plugin =
  tyClDeclTypeNameSpliceWithImports
    (mkFastString "Control.Edit.Plugin.TyDeclNames.plugin")
    (\_ -> EditM . return . (,) () . Just $ (
     [(mkFastString "Control.Edit.Plugin.TyDeclNames.mkNameDecl", mkNameDeclName)]))
    ["import Control.Edit.Plugin.TyDeclNames (mkNameDecl)"]
  where
    mkNameDeclName :: RdrName
    mkNameDeclName =
      mkRdrQual controlEditPluginTyDeclNames $ mkVarOcc "mkNameDecl"

    controlEditPluginTyDeclNames :: ModuleName
    controlEditPluginTyDeclNames =
      mkModuleName "Control.Edit.Plugin.TyDeclNames"

