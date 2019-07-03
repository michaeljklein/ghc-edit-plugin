{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Control.Edit.HsModule.Parse where

import Control.Arrow
import Control.Category (Category(..))
import Control.Edit
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List
import DynFlags
import ErrUtils
import FastString
import GHC
import HeaderInfo
import HscTypes
import Lexer
import Outputable hiding ((<>))
import Panic
import Prelude hiding (id, (.))
import SrcLoc
import StringBuffer
import System.IO
import qualified GHC.Paths
import qualified Parser

-- | Run `Ghc` using `GHC.Paths.libdir`
runGHC :: Ghc a -> IO a
runGHC =
  runGhc $
  Just GHC.Paths.libdir

-- | Parse a Haskell file, given its filename, `DynFlags`, and source code
parse :: String -> DynFlags -> String -> ParseResult (Located (HsModule GhcPs))
parse filename flags str = unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

-- | Parse pragmas in the given `String` and update the given `DynFlags`,
-- given the location (`FilePath`) of the source
parsePragmasIntoDynFlags ::
     DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags flags filepath str =
  catchErrors $ do
    let opts = getOptions flags (stringToStringBuffer str) filepath
    (flags', _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags'
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act =
      handleGhcException reportErr (handleSourceError reportErr act)
    reportErr e = do
      putStrLn $ "error : " ++ show e
      return Nothing

-- | Parse a string
pparseStr ::
     FilePath
  -> String
  -> IO ( DynFlags
        , WarningMessages
        , ErrorMessages
        , Maybe (Located (HsModule GhcPs)))
pparseStr file s = do
  dflags <-
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGHC $ getSessionDynFlags
  (flags, parseResult) <-
    do mflags <- parsePragmasIntoDynFlags dflags file s
       case mflags of
         Nothing -> return (dflags, parse file dflags s)
         Just flags -> return (flags, parse file flags s)
  case parseResult of
    PFailed fs _ _ -> do
      let (wrns, errs) = fs flags
      return (flags, wrns, errs, Nothing)
    POk s' m -> do
      let (wrns, errs) = getMessages s' flags
      return (flags, wrns, errs, Just m)

-- | Parse a Haskell file, returning: `DynFlags`, warnings, errors,
-- and (if it succeeds) a parsed `HsModule`.
pparseIO ::
     FilePath
  -> IO (Either IOException ( DynFlags
                            , WarningMessages
                            , ErrorMessages
                            , Maybe (Located (HsModule GhcPs))))
pparseIO file = do
  hSetEncoding stdout utf8
  tryJust catchInvalidByteSeq . withFile file ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- hGetContents h
    pparseStr file s
  where
    catchInvalidByteSeq :: IOException -> Maybe IOException
    catchInvalidByteSeq e
      | "hGetContents: invalid argument (invalid byte sequence)" `isInfixOf`
          show e = Just e
      | otherwise = Nothing


-- | Edit the `HsModule` at the given path, using the given `Edited`:
--
-- - If it returns `Nothing`, we don't do anything
-- - If it returns `Just`, we update the module
-- - If there's an error, we return `Left` with it shown
--
-- The `HsModule` after any edits is returned.
editHsModule ::
     Edited (Located (HsModule GhcPs))
  -> FilePath
  -> Hsc (Either String (DynFlags, Located (HsModule GhcPs)))
editHsModule (Edited (Kleisli f)) path = do
  parseResult <- liftIO $ pparseIO path
  case parseResult of
    Left err -> return . Left . show $ (path, err)
    ~(Right ~(flags, wrn, err, mlHsModule)) -> do
      case mlHsModule of
        Nothing ->
          return . Left . show $
          (path, fmap (showSDoc flags) $ [wrn, err] >>= pprErrMsgBagWithLoc)
        ~(Just lHsModule) -> do
          mUpdate <- runMaybeT $ f lHsModule
          case mUpdate of
            Nothing -> return $ Right (flags, lHsModule)
            ~(Just lhsModule') -> do
              liftIO . withFile path WriteMode $ \h -> do
                hSetEncoding h utf8
                hPutStr h . showSDoc flags . ppr . unLocated $ lhsModule'
              return $ Right (flags, lhsModule')

