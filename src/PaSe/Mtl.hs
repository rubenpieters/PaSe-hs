{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module PaSe.Mtl where

import PaSe.Types

import Control.Applicative (liftA2)

import Lens.Micro

class LinearTo s f where
  linearTo :: Traversal' s Float -> Duration -> Target -> f ()

sequential :: (Applicative f) => f () -> f () -> f ()
sequential f1 f2 = liftA2 (\_ _ -> ()) f1 f2

class Parallel f where
  liftP2 :: (a -> b -> c) -> f a -> f b -> f c

parallel :: (Parallel f) => f () -> f () -> f ()
parallel f1 f2 = liftP2 (\_ _ -> ()) f1 f2

class IfThenElse f where
  ifThenElse :: f Bool -> f a -> f a -> f a

class Delay f where
  delay :: Float -> f ()

class Set obj f where
  set :: Traversal' obj a -> a -> f ()

class Get obj f where
  get :: Lens' obj a -> f a
