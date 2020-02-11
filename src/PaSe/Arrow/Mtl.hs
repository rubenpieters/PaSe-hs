{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module PaSe.Arrow.Mtl where

import PaSe.Types
import PaSe.Mtl
import PaSe.Arrow.Const

import Control.Arrow

import Lens.Micro

newtype LensA s = LensA { unLensA :: Traversal' s Float }

class LinearToA s a where
  linearToA :: Duration -> Target -> a (LensA s) ()

instance (LinearTo s m) => LinearToA s (Kleisli m) where
  linearToA for to = Kleisli $ \(LensA traversal) ->
    linearTo traversal for to

instance LinearToA s (ConstA [Texture]) where
  linearToA _ _ = ConstA []

instance LinearToA s (ConstA Duration) where
  linearToA dur _ = ConstA dur

class ParallelA a where
  liftP2A :: (i -> (i1, i2)) -> (o1 -> o2 -> o) -> a i1 o1 -> a i2 o2 -> a i o

parallelA :: (ParallelA a) => a i () -> a i () -> a i ()
parallelA a1 a2 = liftP2A (\x -> (x, x)) (\_ _ -> ()) a1 a2

instance (Parallel m) => ParallelA (Kleisli m) where
  liftP2A deconstructI combineO a1 a2 = Kleisli $ \i ->
    let (i1, i2) = deconstructI i
    in liftP2 combineO (runKleisli a1 i1) (runKleisli a2 i2)

instance (Ord a) => ParallelA (ConstA a) where
  liftP2A _ _ (ConstA a1) (ConstA a2) = ConstA (max a1 a2)
