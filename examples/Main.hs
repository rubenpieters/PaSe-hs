{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import Textures
import qualified View as V
import qualified Anims as A
import AnimDefs

import qualified SDL
import SDL.Vect
import Data.Functor.Const
import Data.List (nub)

allTextures :: [String]
allTextures = nub (getConst (moveAnim 0) ++ ["playerAtk2_0.png", "playerAtk2_1.png", "playerAtk2_2.png", "playerAtk2_3.png", "playerAtk2_4.png", "playerAtk2_5.png", "minusOne.png", "menu.png", "slimeHurt0.png", "slimeHurt1.png", "slimeHurt2.png", "slimeHurt3.png", "slimeIdle.png", "bgSection0.png", "bgSection1.png", "bgSection2.png", "bgSection3.png"])


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
