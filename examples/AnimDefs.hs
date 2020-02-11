{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}

module AnimDefs where

import Prelude hiding ((.))

import View
import Sprite
import PaSe

import Control.Arrow
import Control.Category
import Data.List (find, findIndex)
import Data.Functor.Const
import Lens.Micro hiding (set)

-- Menu Animations

menuIntro :: (Applicative f, Parallel f, LinearTo GameView f, Set GameView f) =>
  f ()
menuIntro =
  ( linearTo (menu . y) (For 0.5) (To 60)
    `parallel`
    linearTo (menu . alpha) (For 0.5) (To 255)
    )

menuOutro :: (Applicative f, Parallel f, LinearTo GameView f, Set GameView f) =>
  f ()
menuOutro =
  ( linearTo (menu . y) (For 0.5) (To 40)
    `parallel`
    linearTo (menu . alpha) (For 0.5) (To 0)
    )

-- Player Animations

moveAnim :: (Applicative f, Parallel f, LinearTo GameView f, Delay f, SetTexture GameView f) =>
  Float -> f ()
moveAnim xVal =
  linearTo (player . x) (For 1) (To xVal)
  `parallel`
  frameByFrame (player . texture) 0.1 ["playerRun0.png", "playerRun1.png", "playerRun2.png",  "playerRun3.png", "playerRun4.png", "playerRun0.png", "playerRun1.png", "playerRun2.png",  "playerRun3.png", "playerRun4.png", "playerIdle.png"]

attackAnim :: (Applicative f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f, Parallel f, LinearTo GameView f, LinearToA GameView a, Arrow a, ParallelA a, WithParticle GameView a f) =>
  f ()
attackAnim =
  frameByFrame (player . texture) 0.05 ["playerAtk2_0.png", "playerAtk2_1.png", "playerAtk2_2.png", "playerAtk2_3.png"]
  `sequential`
  ( hitStop `sequential` frameByFrame (player . texture) 0.05 ["playerAtk2_4.png", "playerAtk2_5.png", "playerIdle.png"]
    `parallel`
    slimeHurt
    )

hitStop :: (Functor f, IfThenElse f, Delay f, Get GameView f) =>
  f ()
hitStop =
  ifThenElse
    (fmap (\loc -> loc == 300) (get (player . x)))
    (delay 0.15)
    (delay 0.05)

slimeHurtSheet :: (Applicative f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f) =>
  f ()
slimeHurtSheet = frameByFrame (slime . texture) 0.1 ["slimeHurt0.png", "slimeHurt1.png", "slimeHurt2.png", "slimeHurt3.png", "slimeIdle.png"]

slimeHurt :: (Applicative f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f, Parallel f, LinearTo GameView f, Parallel f, LinearToA GameView a, Arrow a, ParallelA a, WithParticle GameView a f) =>
  f ()
slimeHurt =
  ifThenElse
    (fmap (\loc -> loc == 300) (get (player . x)))
    (delay 0.15 `sequential` (slimeHurtSheet `parallel` minusOneParticleA))
    (delay 0)

minusOneParticleA :: (LinearTo GameView f, Parallel f, LinearToA GameView a, Arrow a, ParallelA a, WithParticle GameView a f) => f ()
minusOneParticleA = let
  particleAnim :: (LinearToA GameView a, Arrow a, ParallelA a) => a Int ()
  particleAnim =
      (arr (\id -> LensA (particles . paId id . _1 . y)) >>> linearToA (For 0.5) (To 320))
      `parallelA`
      (arr (\id -> LensA (particles . paId id . _1 . alpha)) >>> linearToA (For 0.5) (To 0))
  in withParticle (Texture "minusOne.png") (createParticle 390 390) deleteParticle particleAnim

-- Background Animation

scrollingBg :: (Applicative f, Parallel f, LinearTo GameView f, Set GameView f) =>
  Float -> Lens' GameView Sprite -> Lens' GameView Sprite -> f ()
scrollingBg dur sectionA sectionB =
  ( linearTo (sectionA . x) (For dur) (To (-928))
    `parallel`
    linearTo (sectionB . x) (For dur) (To 0)
  )
  `sequential`
  set (sectionA . x) 928
  `sequential`
  scrollingBg dur sectionB sectionA

parallax :: (Monad f, Parallel f, LinearTo GameView f, Set GameView f, Get GameView f) =>
  f ()
parallax =
  scrollingBg 5 bgSection0A bgSection0B
  `parallel`
  scrollingBg 7.5 bgSection1A bgSection1B
  `parallel`
  scrollingBg 10 bgSection2A bgSection2B
  `parallel`
  scrollingBg 15 bgSection3A bgSection3B

-- Helper

frameByFrame :: (Applicative f, Delay f, SetTexture s f) =>
  Lens' s Texture -> Float -> [String] -> f ()
frameByFrame lens time [] = pure ()
frameByFrame lens time (frame:[]) = setTexture lens (Texture frame)
frameByFrame lens time (frame:frames) = setTexture lens (Texture frame) `sequential` go frames
  where
    go (frame:[]) = delay time `sequential` setTexture lens (Texture frame)
    go (frame:frames) = delay time `sequential` setTexture lens (Texture frame) `sequential` go frames

createParticle :: Float -> Float -> Texture -> GameView -> (GameView, Int)
createParticle x y texture view = let
  currentId = view ^. currentParticleId
  particle = Sprite { _x = x, _y = y, _width = 13*2, _height = 16*2, _alpha = 255, _flippedX = False, _texture = texture }
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
