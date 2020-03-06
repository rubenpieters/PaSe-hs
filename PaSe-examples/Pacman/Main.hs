{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import Textures
import qualified View as V
import qualified Anims as A
import AnimDefs

import PaSe (Texture(..))

import qualified SDL
import SDL.Vect
import Data.Functor.Const
import Data.List (nub)

allTextures :: [Texture]
allTextures = nub $
  getConst (pacmanMove undefined undefined) ++
  getConst (dotAnim undefined undefined) ++
  getConst (readyAnim undefined undefined) ++
  map Texture ["dot1.png", "wall.png", "ghost0.png"]

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "PaSe Demo" SDL.defaultWindow { SDL.windowInitialSize = V2 600 600 }
  counter <- mkCounter
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  textures <- loadTextures allTextures renderer
  gameLoop renderer textures V.initialView A.initialAnims counter
  destroyTextures textures
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
