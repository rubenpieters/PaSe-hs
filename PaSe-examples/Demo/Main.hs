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
  getConst (moveAnim 0) ++
  getConst (attackAnim) ++
  map Texture ["bgSection0.png", "bgSection1.png", "bgSection2.png", "bgSection3.png", "menu.png"]

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "PaSe Demo" SDL.defaultWindow { SDL.windowInitialSize = V2 500 500 }
  counter <- mkCounter
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  textures <- loadTextures allTextures renderer
  gameLoop renderer textures V.initialView A.initialAnims counter
  destroyTextures textures
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
