{-# LANGUAGE RankNTypes #-}

module PaSe.Prelude where

import PaSe.Mtl
import PaSe.Types

import Lens.Micro

frameByFrame :: (Applicative f, Delay f, SetTexture s f) =>
  Lens' s Texture -> Float -> [String] -> f ()
frameByFrame lens time [] = pure ()
frameByFrame lens time (frame:[]) = setTexture lens (Texture frame)
frameByFrame lens time (frame:frames) = setTexture lens (Texture frame) `sequential` go frames
  where
    go (frame:[]) = delay time `sequential` setTexture lens (Texture frame)
    go (frame:frames) = delay time `sequential` setTexture lens (Texture frame) `sequential` go frames
