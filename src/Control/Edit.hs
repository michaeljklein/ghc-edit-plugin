{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Edit where

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import GHC
import HscTypes

-- | `Nothing` if no change
type EditM = MaybeT Hsc

infixr 0 >-
-- | `Hsc` edits (return `Nothing` if not changed)
--
-- This type is equivalent to: @LensLike' EditM a b@
type (>-) a b = (a -> EditM a) -> b -> EditM b

-- | A monadic endomorphism, i.e. a `Kleisli` endomorphism,
-- over `EditM`.
newtype Edited a = Edited { runEdited :: Kleisli EditM a a }

-- | This instance composes edits of the same type
instance Semigroup (Edited a) where
  Edited (Kleisli f) <> Edited (Kleisli g) =
    Edited . Kleisli $ \x ->
      MaybeT $ do
        y <- runMaybeT $ f x
        case y of
          Nothing -> runMaybeT $ g x
          ~(Just z) -> Just . fromMaybe z <$> runMaybeT (g x)

instance Monoid (Edited a) where
  mempty = Edited . Kleisli . const . MaybeT . return $ Nothing

-- | Perform an action that may depend on the value, but don't edit anything
effectEdited :: (a -> Hsc b) -> Edited a
effectEdited f = Edited . Kleisli $ \x -> lift (f x) >> empty

-- | Perform an edit on a `Functor`, given a F-algebra
editedFAlg :: Functor f => (f a -> a) -> Edited a -> Edited (f a)
editedFAlg f (Edited (Kleisli g)) = Edited . Kleisli $ \x -> MaybeT $ do
  y <- runMaybeT $ g (f x)
  return $ (<$ x) <$> y

-- | Edit, ignoring `Located`
editedLocated :: Edited a -> Edited (Located a)
editedLocated = editedFAlg unLocated

-- | Run a `(>-)` on an `Edited`
runEdit :: a >- b -> Edited a -> Edited b
runEdit f = Edited . Kleisli . f . runKleisli . runEdited

-- | Edit a list using `patchList`
editList :: a >- b -> a >- [b]
editList f g xs = MaybeT $ do
  ys <- mapM (runMaybeT . f g) xs
  return $ patchList xs ys

-- | The list to patch must be at least as long as the list to patch with
--
-- Equivalent to:
--
-- @
-- \xs ys -> if any isJust ys
--   then Just $ zipWith fromMaybe xs ys
--   else Nothing
-- @
patchList :: [a] -> [Maybe a] -> Maybe [a]
patchList [] [] = Nothing
patchList _  [] = Nothing
patchList [] _  = error "list to patch shorter than list to patch with"
patchList ~(x:xs) ~(y:ys) =
  case y of
    Nothing -> (x :) <$> patchList xs ys
    ~(Just z) -> Just $ z : zipWith fromMaybe xs ys

-- | Note: this simply collects edits as single edits: it's not equivalent to editList for lists
editT :: Traversable f => a >- b -> a >- f b
editT f g xs = mapM (f g) xs

-- | Edit by monadically inserting values after each given value
editConcatMapAfterM :: (a -> EditM [a]) -> [a] >- b -> Edited b
editConcatMapAfterM f g = Edited . Kleisli $ \x -> (g (\y -> concat <$> mapM (\z -> (z :) <$> f z) y) x)

-- | Edit, ignoring `Located`. (See `editedLocated`)
editLocated :: a >- b -> a >- Located b
editLocated = editT

-- | Drop the location of a `Located` value
unLocated :: Located a -> a
unLocated ~(L _ x) = x

