{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import PaSe (parallel)

import View (GameView)
import qualified View as V
import Anims (GameAnims)
import qualified Anims as A
import Textures
import AnimDefs
import Sprite
import Types
import Field

import Control.Monad (unless)
import Data.Word
import qualified Data.Map as Map
import qualified SDL
import qualified SDL.Raw.Timer as SDL.Timer
import SDL (($=))
import SDL.Vect

import Lens.Micro

import Debug.Trace

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
      wPressed = any (eventIsPress SDL.KeycodeW) events
      aPressed = any (eventIsPress SDL.KeycodeA) events
      sPressed = any (eventIsPress SDL.KeycodeS) events
      dPressed = any (eventIsPress SDL.KeycodeD) events
      spcPressed = any (eventIsPress SDL.KeycodeSpace) events
  (animsU, viewU) <- case (anims ^. A.readyAnims) of
    Just _ -> return (anims, view)
    Nothing -> do
      -- info
      let pacmanLoc = V.pacmanLoc (view ^. V.pacmanSprite)
          thisTile = tileValue (view ^. V.field) pacmanLoc
          nextTile = tileInDir pacmanLoc (view ^. V.pacmanExtra . V.moveDir) (view ^. V.field)
          eatDot = thisTile == Just Dot
      -- update game view
      let view1 = case (wPressed, aPressed, sPressed, dPressed) of
            (True, _, _, _) -> view & V.pacmanExtra . V.moveDir .~ DirUp
            (_, True, _, _) -> view & V.pacmanExtra . V.moveDir .~ DirLeft
            (_, _, True, _) -> view & V.pacmanExtra . V.moveDir  .~ DirDown
            (_, _, _, True) -> view & V.pacmanExtra . V.moveDir  .~ DirRight
            (_, _, _, _) -> view
      let view2 = case (eatDot) of
            True -> view1 & V.field %~ Map.delete pacmanLoc
            False -> view1
      -- update animations
      let anims1 = case (eatDot) of
            True -> anims & A.particleAnims %~ addInMaybe parallel (dotAnim (view ^. V.pacmanSprite . x) (view ^. V.pacmanSprite . y))
            False -> anims
      let anims2 = case (anims1 ^. A.pacmanMoveAnims, nextTile) of
            (Nothing, x) | x /= Just Wall -> anims1 & A.pacmanMoveAnims .~ Just (pacmanMove view (view ^. V.pacmanExtra . V.moveDir))
            (Nothing, Just Wall) -> anims1 & A.pacmanMoveAnims .~ Just (pacmanRotate (view ^. V.pacmanExtra . V.moveDir))
            _ -> anims1
      return (anims2, view2)
  -- update game with animations
  let (viewA, animsA) = A.runAllAnimations deltaTime (viewU, animsU)
  -- draw game
  drawGame viewA renderer textures
  -- loop or quit
  unless qPressed (gameLoop renderer textures viewA animsA counter')

addIfNothing :: Maybe a -> a -> Maybe a
addIfNothing (Just a) _ = Just a
addIfNothing Nothing a = Just a

addInMaybe :: (a -> a -> a) -> a -> Maybe a ->  Maybe a
addInMaybe f a2 (Just a1) = Just (f a1 a2)
addInMaybe _ a Nothing = Just a
