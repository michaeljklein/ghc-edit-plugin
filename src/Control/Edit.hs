{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Control.Edit where

import Control.Applicative
import Control.Arrow (Kleisli(..))
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.Maybe
import Data.Set (Set)
import Data.Set.Utils
import GHC
import HscTypes
import Plugins
import qualified Data.Set as Set

-- | Context of editing a Haskell module
data EditCxt = EditCxt
  { ecCommandLineOptions :: [CommandLineOption]
  , ecModSummary         :: ModSummary
  , ecSrcFiles           :: [FilePath]
  , ecAnnotations        :: ApiAnns
  }

-- | @a@ is the return value and @t@ is the type being edited, or `Nothing` if no change
newtype EditM a t = EditM
  { runEditM :: ReaderT EditCxt Hsc (a, Maybe t)
  } deriving (Functor)

-- | Don't change anything
noEditM :: a -> EditM a t
noEditM = EditM . ReaderT . const . return . (, Nothing)

instance Bifunctor EditM where
  bimap f g = EditM . fmap (bimap f (fmap g)) . runEditM
  first f = EditM . fmap (first f) . runEditM
  second = fmap


infixr 0 >-
-- | `Hsc` edits (return `Nothing` if not changed)
--
-- This type is equivalent to: @LensLike' EditM a b@
type (>-) s t = forall a. (s -> EditM a s) -> t -> EditM a t

-- | An edit of a @t@ype, along with a return value: @a@
newtype EditedM t a = EditedM
  { runEditedM :: Kleisli (EditM a) t t
  }

-- | Get the `EditCxt`
getEditedCxt :: EditedM t EditCxt
getEditedCxt = EditedM . Kleisli . const . EditM $ (, Nothing) <$> ask

-- | Get the value being edited
getEditedValue :: EditedM t t
getEditedValue = EditedM $ Kleisli noEditM

-- | Perform an action that may depend on the value, but don't edit anything
effectEdited :: (s -> ReaderT EditCxt Hsc a) -> EditedM s a
effectEdited f = EditedM . Kleisli $ \x -> EditM . fmap (, Nothing) $ f x

hoistKleisli :: (forall x. m x -> n x) -> Kleisli m a b -> Kleisli n a b
hoistKleisli f = Kleisli . (f <$>) . runKleisli

instance Functor (EditedM t) where
  fmap f = EditedM . hoistKleisli (first f) . runEditedM

instance Applicative (EditedM t) where
  pure = EditedM . Kleisli . fmap (EditM . return . fmap Just) . (,)
  EditedM (Kleisli fs) <*> EditedM (Kleisli xs) =
    EditedM . Kleisli $ \x ->
      EditM $ do
        ~(f, y) <- runEditM $ fs x
        runEditM . first f $ maybe (xs x) xs y

instance Monad (EditedM t) where
  EditedM (Kleisli xs) >>= f =
    EditedM . Kleisli $ \x ->
      EditM $ do
        ~(xs', y) <- runEditM $ xs x
        maybe
          ((fmap runEditM . runKleisli . runEditedM $ f xs') x)
          (fmap runEditM . runKleisli . runEditedM $ f xs')
          y

instance Semigroup a => Semigroup (EditedM t a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (EditedM t a) where
  mempty = return mempty

-- | Perform an edit on a `Functor`, given a F-algebra
editedFAlg :: Functor f => (f s -> s) -> EditedM s a -> EditedM (f s) a
editedFAlg f (EditedM (Kleisli g)) = EditedM . Kleisli $ \x -> EditM $ do
  ~(y, x') <- runEditM $ g (f x)
  return (y, (<$ x) <$> x') -- (<$ x) <$> y

-- | Edit, ignoring `Located`
editedLocated :: EditedM s a -> EditedM (Located s) a
editedLocated = editedFAlg unLocated

-- | Run a `(>-)` on an `Edited`
runEdit :: s >- t -> EditedM s a -> EditedM t a
runEdit f = EditedM . Kleisli . f . runKleisli . runEditedM

---- | Edit a list using `patchList`
--editList :: a >- b -> a >- [b]
--editList f g xs = EditM $ do
--  ys <- mapM (runEditM . f g) xs
--  _ xs ys
--  -- return $ patchList xs ys
--
---- | The list to patch must be at least as long as the list to patch with
----
---- Equivalent to:
----
---- @
---- \xs ys -> if any isJust ys
----   then Just $ zipWith fromMaybe xs ys
----   else Nothing
---- @
--patchList :: [a] -> [Maybe a] -> Maybe [a]
--patchList [] [] = Nothing
--patchList _  [] = Nothing
--patchList [] _  = error "list to patch shorter than list to patch with"
--patchList ~(x:xs) ~(y:ys) =
--  case y of
--    Nothing -> (x :) <$> patchList xs ys
--    ~(Just z) -> Just $ z : zipWith fromMaybe xs ys

maybeNonEmpty :: [a] -> Maybe [a]
maybeNonEmpty = \case
  [] -> Nothing
  xs -> Just xs

-- | Convert to index,
-- convert from index with the previous value (unless not found in list to edit),
-- set of indices to merge
--
-- If @toIndex x \``elem`\` indices@, remove from @indices@
-- - Continue for all @x@
-- - Append (prepend) remaining to list, converting with @fromIndex@
editedMergeWithIndices ::
     Ord i => (s -> i) -> (Maybe s -> i -> s) -> Set i -> EditedM [s] ()
editedMergeWithIndices toIndex fromIndex indices =
  void . EditedM . Kleisli $ \xs -> EditM $ do
    ~(addedXs, ys) <- fmap (bimap (fmap (fromIndex Nothing) . Set.toAscList) Just) $ do
      foldM (\ ~(indices', addedXs) x ->
        case toIndex x `lookupSetValue` indices' of
          Nothing -> return (indices', addedXs ++ [x])
          ~(Just ~(y, indices'')) -> do
            return (indices'', addedXs ++ [Just x `fromIndex` y])
        )
        (indices, []) xs
    return . (,) () $ maybe (maybeNonEmpty addedXs) (Just . (++ addedXs)) ys

-- | Note: this simply collects edits as single edits: it's not equivalent to editList for lists
-- editT :: Traversable f => a >- b -> a >- f b
-- editT f g xs = EditM . fmap _ $ mapM (runEditM . f g) xs

-- | Edit by monadically inserting values after each given value
editConcatMapAfterM :: (s -> EditM a [s]) -> [s] >- t -> EditedM t [a]
editConcatMapAfterM f g =
  EditedM . Kleisli $ \x ->
    g
      (EditM .
       fmap
         (fmap
            ((\case
                [] -> Nothing
                xs -> Just xs) .
             concat . catMaybes) .
          unzip) .
       mapM (runEditM . f))
      x

-- | Edit, ignoring `Located`. (See `editedLocated`)
-- editLocated :: a >- b -> a >- Located b
-- editLocated = editT

-- | Drop the location of a `Located` value
unLocated :: Located a -> a
unLocated ~(L _ x) = x

