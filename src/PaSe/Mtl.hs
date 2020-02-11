{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module PaSe.Mtl where

import PaSe.Types

import Control.Applicative (liftA2)
import Data.Functor.Const

import Lens.Micro

-- LinearTo

class LinearTo s f where
  linearTo :: Traversal' s Float -> Duration -> Target -> f ()

sequential :: (Applicative f) => f () -> f () -> f ()
sequential f1 f2 = liftA2 (\_ _ -> ()) f1 f2

instance LinearTo s (Const [Texture]) where
  linearTo _ _ _ = Const []

instance LinearTo s (Const Duration) where
  linearTo _ dur _ = Const dur

-- Parallel

class Parallel f where
  liftP2 :: (a -> b -> c) -> f a -> f b -> f c

parallel :: (Parallel f) => f () -> f () -> f ()
parallel f1 f2 = liftP2 (\_ _ -> ()) f1 f2

instance Semigroup m => Parallel (Const m) where
  liftP2 _ (Const a) (Const b) = Const (a <> b)

-- IfThenElse

class IfThenElse f where
  ifThenElse :: f Bool -> f a -> f a -> f a

instance Semigroup m => IfThenElse (Const m) where
  ifThenElse (Const cond) (Const ifBranch) (Const elseBranch) =
    Const (cond <> ifBranch <> elseBranch)

-- Delay

class Delay f where
  delay :: Float -> f ()

instance Monoid m => Delay (Const m) where
  delay _ = Const mempty

-- Set

class Set s f where
  set :: Traversal' s a -> a -> f ()

-- SetTexture

class SetTexture s f where
  setTexture :: Traversal' s Texture -> Texture -> f ()

instance SetTexture s (Const [Texture]) where
  setTexture _ texture = Const [texture]

-- Get

class Get s f where
  get :: Lens' s a -> f a

instance Monoid m => Get s (Const m) where
  get _ = Const mempty

