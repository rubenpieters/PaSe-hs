{-# LANGUAGE TemplateHaskell #-}

module Sprite where

import Textures

import qualified SDL
import SDL (($=))
import SDL.Vect

import Lens.Micro
import Lens.Micro.TH

data Sprite = Sprite
  { _x :: Float
  , _y :: Float
  , _width :: Float
  , _height :: Float
  , _alpha :: Float -- 0 (transparent) - 255 (opaque)
  , _flippedX :: Bool
  , _texture :: String
  }

makeLenses ''Sprite

drawSprite :: Sprite -> SDL.Renderer -> Textures -> IO ()
drawSprite sprite renderer textures = let
  position = SDL.P (V2 (fromIntegral (floor (sprite ^. x) :: Int)) (fromIntegral (floor (sprite ^. y) :: Int)))
  dimension = V2 (fromIntegral (floor (sprite ^. width) :: Int)) (fromIntegral (floor (sprite ^. height) :: Int))
  textureName = sprite ^. texture
  sdlTexture =  textureFromMap textures textureName
  in do
  case sprite ^. alpha <= 254 of
    True -> do
      SDL.textureBlendMode sdlTexture $= SDL.BlendAlphaBlend
      SDL.textureAlphaMod sdlTexture $= fromIntegral (floor (sprite ^. alpha) :: Int)
    False -> return ()
  case sprite ^. flippedX of
    True -> SDL.copyEx renderer sdlTexture Nothing (Just (SDL.Rectangle position dimension)) 0 Nothing (V2 True False)
    False -> SDL.copy renderer sdlTexture Nothing (Just (SDL.Rectangle position dimension))

