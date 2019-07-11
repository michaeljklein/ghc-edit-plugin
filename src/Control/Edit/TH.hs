{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Control.Edit.TH where

-- import Control.Edit.HsModule
import Control.Edit
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import GHC
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH as TH

editRecordField :: (t -> s) -> (t -> s -> t) -> s >- t
editRecordField getField' putField' f xs =
  EditM . (fmap . fmap . fmap) (putField' xs) . runEditM . f $ getField' xs

-- | Resulting type is: @(record -> field -> record)@
putRecordFieldE :: TH.Name -> Q Exp
putRecordFieldE fieldName = do
  recVarName <- newName "recVar"
  fieldVarName <- newName "fieldVar"
  lamE [varP recVarName, varP fieldVarName] $
    recUpdE (varE recVarName) [(,) fieldName <$> varE fieldVarName]

editRecordConField :: TH.Name -> TH.Name -> Q Dec
editRecordConField conName fieldName = do
  let editorName = mkName $ "edit" ++ nameBase conName ++ nameBase fieldName
  valD
    (varP editorName)
    (normalB $
     [|editRecordField|] `appE` varE fieldName `appE` putRecordFieldE fieldName)
    []

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

