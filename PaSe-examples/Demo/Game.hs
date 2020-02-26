{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import View (GameView)
import qualified View as V
import Anims (GameAnims)
import qualified Anims as A
import Textures
import AnimDefs
import Sprite

import Control.Monad (unless)
import Data.Word
import qualified SDL
import qualified SDL.Raw.Timer as SDL.Timer
import SDL (($=))
import SDL.Vect

import Lens.Micro

data Counter = Counter
  { now :: Word64
  , prev :: Word64
  }

mkCounter :: IO Counter
mkCounter = do
  now <- SDL.Timer.getPerformanceCounter
  return (Counter now now)

drawGame :: GameView -> SDL.Renderer -> Textures -> IO ()
drawGame view renderer textures = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer
  V.drawView view renderer textures
  SDL.present renderer

gameLoop :: SDL.Renderer -> Textures -> GameView -> GameAnims -> Counter -> IO ()
gameLoop renderer textures view anims Counter{ now, prev } = do
  -- calculate delta time
  freq <- SDL.Timer.getPerformanceFrequency
  let deltaTime :: Float = (fromIntegral now - fromIntegral prev) / fromIntegral freq
  -- update counter
  now' <- SDL.Timer.getPerformanceCounter
  let counter' = Counter now' now
  -- get input
  events <- SDL.pollEvents
  let eventIsPress key event = case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
          SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == key
        _ -> False
      qPressed = any (eventIsPress SDL.KeycodeQ) events
      dPressed = any (eventIsPress SDL.KeycodeD) events
      aPressed = any (eventIsPress SDL.KeycodeA) events
      mPressed = any (eventIsPress SDL.KeycodeM) events
      ePressed = any (eventIsPress SDL.KeycodeE) events
      spcPressed = any (eventIsPress SDL.KeycodeSpace) events
  -- update game view
  let view1 = case (aPressed, dPressed, anims ^. A.player) of
        (True, _, Nothing) -> view & V.player . flippedX .~ True
        (_, True, Nothing) -> view & V.player . flippedX .~ False
        (_, _, _) -> view
  let view2 = case (ePressed) of
        True -> view1 & V.bgMoving %~ not
        _ -> view1
  let viewU = view2
  -- update animations
  let anims1 = case (aPressed, dPressed, viewU ^. V.player . x, spcPressed) of
        (True, _, 300, _) -> anims & A.player %~ \l -> addIfNothing l (moveAnim ((viewU ^. V.player . x) - 250))
        (_, True, 50, _) -> anims & A.player %~ \l -> addIfNothing l (moveAnim ((viewU ^. V.player . x) + 250))
        (_, _, _, True) -> anims & A.player %~ \l -> addIfNothing l attackAnim
        (_, _, _, _) -> anims
  let menuVisible = viewU ^. V.menu . alpha > 254
      anims2 = case (mPressed, menuVisible) of
        (True, False) -> anims1 & A.menu %~ \l -> addIfNothing l menuIntro
        (True, True) -> anims1 & A.menu %~ \l -> addIfNothing l menuOutro
        (False, _) -> anims1
  -- update game with animations
  let (viewA, animsA) = A.runAllAnimations deltaTime (viewU, anims2)
  -- draw game
  drawGame viewU renderer textures
  -- loop or quit
  unless qPressed (gameLoop renderer textures viewA animsA counter')

addIfNothing :: Maybe a -> a -> Maybe a
addIfNothing (Just a) _ = Just a
addIfNothing Nothing a = Just a
