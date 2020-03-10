{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module AnimDefs where

import Prelude hiding ((.), id)

import Types
import View
import Sprite

import PaSe
import PaSe.Prelude

import Control.Arrow
import Control.Category
import Data.List (find, findIndex)
import Data.Functor.Const

import Lens.Micro hiding (set)

pacmanRotate :: (Applicative f, Parallel f, Delay f, SetTexture GameView f, LinearTo GameView f, Set GameView f) =>
  Dir -> f ()
pacmanRotate dir = let
  newRotation DirUp = 270
  newRotation DirDown = 90
  newRotation DirLeft = 180
  newRotation DirRight = 0
  in set (pacmanSprite . rotation) (newRotation dir)

pacmanMove :: (Applicative f, Parallel f, Delay f, SetTexture GameView f, LinearTo GameView f, Set GameView f) =>
  GameView -> Dir -> f ()
pacmanMove view dir = let
  l = ["pacman0.png", "pacman1.png", "pacman2.png", "pacman1.png", "pacman0.png"]
  current = view ^. pacmanSprite . moveLens dir
  in
    pacmanRotate dir
    `parallel`
    frameByFrame (pacmanSprite . texture) 0.05 l
    `parallel`
    linearTo (pacmanSprite . moveLens dir) (For 0.2) (To (current + moveOffset dir))

ghostMove :: (Applicative f, Parallel f, Delay f, SetTexture GameView f, LinearTo GameView f, Set GameView f) =>
  GameView -> Dir -> f ()
ghostMove view dir = let
  current = view ^. ghostSprite . moveLens dir
  in
    linearTo (ghostSprite . moveLens dir) (For 0.2) (To (current + moveOffset dir))

deathAnim :: (Applicative f, Parallel f, Delay f, SetTexture GameView f, LinearTo GameView f, Set GameView f) =>
  f ()
deathAnim = let
  l = ["pacman1.png", "pacman2.png", "pacman1.png", "pacman2.png"]
  in
    frameByFrame (pacmanSprite . texture) 0.15 l

moveLens :: Dir -> Lens' Sprite Float
moveLens DirUp = y
moveLens DirDown = y
moveLens DirLeft = x
moveLens DirRight = x

moveOffset :: Dir -> Float
moveOffset DirUp = -30
moveOffset DirDown = 30
moveOffset DirLeft = -30
moveOffset DirRight = 30

dotAnim :: (LinearTo GameView f, Parallel f, LinearToA GameView a, Arrow a, ParallelA a, WithParticle GameView a f) =>
  Float -> Float -> f ()
dotAnim pX pY = let
  particleAnim :: (LinearToA GameView a, Arrow a, ParallelA a) => a Int ()
  particleAnim =
      (arr (\id -> LensA (particles . paId id . _1 . y)) >>> linearToA (For 1.5) (To (pY - 30)))
      `parallelA`
      (arr (\id -> LensA (particles . paId id . _1 . alpha)) >>> linearToA (For 1.5) (To 0))
  in withParticle (Texture "100.png") (createParticle pX pY (20*2) (12*2)) deleteParticle particleAnim

readyAnim :: (LinearTo GameView f, Parallel f, LinearToA GameView a, Arrow a, ParallelA a, WithParticle GameView a f) =>
  Float -> Float -> f ()
readyAnim pX pY = let
  particleAnim :: (LinearToA GameView a, Arrow a, ParallelA a) => a Int ()
  particleAnim =
      (arr (\id -> LensA (particles . paId id . _1 . y)) >>> linearToA (For 1.5) (To (pY - 30)))
      `parallelA`
      (arr (\id -> LensA (particles . paId id . _1 . alpha)) >>> linearToA (For 1.5) (To 0))
  in withParticle (Texture "ready.png") (createParticle pX pY (50*2) (10*2)) deleteParticle particleAnim

-- Helper

createParticle :: Float -> Float -> Float -> Float -> Texture -> GameView -> (GameView, Int)
createParticle x y width height texture view = let
  currentId = view ^. currentParticleId
  particle = Sprite { _x = x, _y = y, _width = width, _height = height, _alpha = 255, _flippedX = False, _flippedY = False, _rotation = 0, _texture = texture }
  newView = view & particles %~ (\l -> (particle, currentId) : l)
                 & currentParticleId %~ (+ 1)
  in (newView, currentId)

deleteParticle :: Int -> GameView -> GameView
deleteParticle id view = let
  isNotId x = x ^. _2 /= id
  in view & particles .~ filter isNotId (view ^. particles)

paId :: Int -> Lens' [(Sprite, Int)] (Sprite, Int)
paId i = let
  get l = case find (\x -> x ^. _2 == i) l of
    Just s -> s
    Nothing -> error ("no particle with index " ++ show i)
  set l x = case findIndex (\x -> x ^. _2 == i) l of
    Just ix -> take ix l ++ x : drop (ix+1) l
    Nothing -> error ("no particle with index " ++ show i)
  in lens get set

class WithParticle s a f | f -> a where
  withParticle :: Texture -> (Texture -> s -> (s, Int)) -> (Int -> s -> s) -> a Int () -> f ()

instance (Monad m) => WithParticle s (Kleisli (Animation s m)) (Animation s m) where
  withParticle texture createParticle deleteParticle a = Animation $ \s t -> let
    (s', id) = createParticle texture s
    remove del i = Animation $ \s t -> pure (del i s, Right (), Just t)
    in runAnimation (runKleisli a id `sequential` remove deleteParticle id) s' t

instance WithParticle s (ConstA [Texture]) (Const [Texture]) where
  withParticle texture _ _ a = Const (texture : getConstA a)
