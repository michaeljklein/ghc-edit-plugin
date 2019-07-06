{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Control.Edit.TH where

import GHC
import Control.Edit
import Control.Monad
-- import Control.Edit.HsModule
import Control.Monad.Trans.Maybe
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Maybe

-- editRecordConField :: TH.Name -> TH.Name -> Q Dec
-- editRecordConField = _

editRecordConField :: TH.Name -> TH.Name -> Q Dec
editRecordConField conName fieldName = do
  let editorName = mkName $ "edit" ++ nameBase conName ++ nameBase fieldName
  fName <- newName "f"
  xsName <- newName "xs"
  ysName <- newName "ys"
  funD editorName [
    clause
      [varP fName, varP xsName]
      (normalB (appE
                  [e|MaybeT|]
                  (appE
                     (appE
                        [|fmap|]
                        (appE
                           [|fmap|]
                           (lamE
                              [varP ysName]
                              (recUpdE
                                 (varE xsName)
                                 [return (fieldName, VarE ysName)]))))
                     (appE
                        [e|runMaybeT|]
                        (appE
                           (unboundVarE fName)
                           (appE (varE fieldName) (varE xsName))
                        )
                     )
                  )
               )
      )
      []
    ]

editRecordFieldHelper :: String
editRecordFieldHelper = $(do
  xs <- [e| \xs -> MaybeT
                (fmap
                  (fmap (\ys -> xs {hsmodDecls = ys}))
                  (runMaybeT (f (hsmodDecls xs)))
                )
              |]
  stringE $ show xs)


fst3 :: (a, b, c) -> a
fst3 ~(x, _, _) = x

editRecordConFields :: TH.Name -> [VarBangType] -> Q [Dec]
editRecordConFields conName fields =
  mapM (editRecordConField conName . fst3) fields

maybeRecC :: Con -> Maybe (TH.Name, [VarBangType])
maybeRecC (RecC name fields) = Just (name, fields)
maybeRecC _ = Nothing

maybeRecCs :: [Con] -> [(TH.Name, [VarBangType])]
maybeRecCs = mapMaybe maybeRecC

editRecordFields :: TH.Name -> Q [Dec]
editRecordFields recordName = do
  recordInfo <- reify recordName
  case recordInfo of
    TyConI recordDec ->
      case recordDec of
        DataD _ recordName' _ _ constructors _ -> do
          when (recordName /= recordName') . fail $
            unwords
              [ "editRecordFields: given recordName:"
              , show recordName
              , "/= DataD recordName:"
              , show recordName'
              ]
          concat <$>
            mapM (uncurry editRecordConFields) (maybeRecCs constructors)
        _ -> fail "editRecordFields: Expected DataD"
    _ -> fail "editRecordFields: Expected TyConI"

-- | Edit the `LHsDecl`'s in a `HsModule`
editHsmodDecls :: [LHsDecl pass] >- HsModule pass
editHsmodDecls f HsModule {..} =
  MaybeT $ do
    mhsmodDecls <- runMaybeT $ f hsmodDecls
    return $ do
      hsmodDecls' <- mhsmodDecls
      return $ HsModule {hsmodDecls = hsmodDecls', ..}

