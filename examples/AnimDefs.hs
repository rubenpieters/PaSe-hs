{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AnimDefs where

import View
import Sprite
import PaSe

import Data.List (find, findIndex)
import Data.Functor.Const
import Lens.Micro hiding (set)

class SetTexture s f where
  setTexture :: Traversal' s String -> String -> f ()

instance (Applicative m) => SetTexture s (Animation s m) where
  setTexture = set

class Create s f where
  create :: (s -> (s, Int)) -> f Int

instance (Applicative m) => Create s (Animation s m) where
  create f = Animation $ \s t -> let
    (newS, index) = f s
    in pure (newS, Right index, Just t)

class Delete s f where
  delete :: (s -> s) -> f ()

instance (Applicative m) => Delete s (Animation s m) where
  delete f = Animation $ \s t ->
    pure (f s, Right (), Just t)

instance SetTexture s (Const [String]) where
  setTexture _ texture = Const [texture]

instance Monoid m => Create s (Const m) where
  create _ = Const mempty

instance Monoid m => Delete s (Const m) where
  delete _ = Const mempty

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

attackAnim :: (Monad f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f, Parallel f, LinearTo GameView f, Create GameView f, Delete GameView f) =>
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

slimeHurt :: (Monad f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f, Delay f, Parallel f, Create GameView f, Delete GameView f, LinearTo GameView f) =>
  f ()
slimeHurt =
  ifThenElse
    (fmap (\loc -> loc == 300) (get (player . x)))
    (delay 0.15 `sequential` (slimeHurtSheet `parallel` minusOneParticle))
    (delay 0)

minusOneParticle :: (Monad f, LinearTo GameView f, Create GameView f, Delete GameView f, Parallel f) =>
  f ()
minusOneParticle = do
  id <- create (createParticle 390 390 "minusOne.png")
  ( linearTo (particles . paId id . _1 . y) (For 0.5) (To 320)
    `parallel`
    linearTo (particles . paId id . _1 . alpha) (For 0.5) (To 0)
    )
  delete (deleteParticle id)

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
  Lens' s String -> Float -> [String] -> f ()
frameByFrame lens time [] = pure ()
frameByFrame lens time (frame:[]) = setTexture lens frame
frameByFrame lens time (frame:frames) = setTexture lens frame `sequential` go frames
  where
    go (frame:[]) = delay time `sequential` setTexture lens frame
    go (frame:frames) = delay time `sequential` setTexture lens frame `sequential` go frames

createParticle :: Float -> Float -> String -> GameView -> (GameView, Int)
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
