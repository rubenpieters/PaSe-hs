{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import PaSe

import qualified SDL
import qualified SDL.Image
import qualified SDL.Raw.Timer as SDL.Timer
import SDL (($=))
import SDL.Vect
import Control.Monad (unless)
import Control.Monad.Identity
import qualified Control.Monad.State as S
import Data.List (find, findIndex)
import Data.Word
import Data.Functor.Const
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.List (nub)

import Lens.Micro hiding (set)
import Lens.Micro.TH

class SetTexture s f where
  setTexture :: Traversal' s String -> String -> f ()

instance (Applicative m) => SetTexture s (Animation s m) where
  setTexture lens texture = set lens texture

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

instance Monoid m => Get s (Const m) where
  get _ = Const mempty

instance Semigroup m => Parallel (Const m) where
  liftP2 _ (Const a) (Const b) = Const (a <> b)

instance Monoid m => Delay (Const m) where
  delay _ = Const mempty

instance Monoid m => LinearTo s (Const m) where
  linearTo _ _ _ = Const mempty

instance Semigroup m => IfThenElse (Const m) where
  ifThenElse (Const cond) (Const ifBranch) (Const elseBranch) =
    Const (cond <> ifBranch <> elseBranch)

instance Monoid m => Create s (Const m) where
  create _ = Const mempty

instance Monoid m => Delete s (Const m) where
  delete _ = Const mempty

data BgCounters = BgCounters
  { _moving :: Bool
  , _counter1 :: Float
  , _flipped1 :: Bool
  , _counter2 :: Float
  , _flipped2 :: Bool
  , _counter3 :: Float
  , _flipped3 :: Bool
  , _counter4 :: Float
  , _flipped4 :: Bool
  }

makeLenses ''BgCounters

data GameView = GameView
  { _vPlayer :: (Float, Float, String, Bool)
  , _vSlime :: (Float, Float, String)
  , _vParticles :: [(Float, Float, String, Int, Float)]
  , _currentParticleId :: Int
  , _vBackground :: ()
  , _vMenu :: (Float, Float, Float, Bool)
  , _bgCounters :: BgCounters
  }

makeLenses ''GameView

data GameAnims = GameAnims
  { _aPlayer :: Maybe (Animation GameView Identity ())
  , _aSlime :: Maybe (Animation GameView Identity ())
  , _aParticles :: Maybe (Animation GameView Identity ())
  , _aBackground :: Maybe (Animation GameView Identity ())
  , _aMenu :: Maybe (Animation GameView Identity ())
  }

makeLenses ''GameAnims

initialView :: GameView
initialView = GameView
  { _vPlayer = (50, 365, "playerIdle.png", False)
  , _vSlime = (370, 390, "slimeIdle.png")
  , _vParticles = []
  , _currentParticleId = 0
  , _vBackground = ()
  , _vMenu = (100, 80, 0, False)
  , _bgCounters = BgCounters
    { _moving = False
    , _counter1 = 0
    , _flipped1 = False
    , _counter2 = 0
    , _flipped2 = False
    , _counter3 = 0
    , _flipped3 = False
    , _counter4 = 0
    , _flipped4 = False
    }
  }

initialAnims :: GameAnims
initialAnims = GameAnims
  { _aPlayer = Nothing
  , _aSlime = Nothing
  , _aParticles = Nothing
  , _aBackground = Just (scrollingBg 5 (bgCounters . counter1) (bgCounters . flipped1) `parallel` scrollingBg 7.5 (bgCounters . counter2) (bgCounters . flipped2) `parallel` scrollingBg 10 (bgCounters . counter3) (bgCounters . flipped3) `parallel` scrollingBg 15 (bgCounters . counter4) (bgCounters . flipped4))
  , _aMenu = Just menuIntro
  }

createParticle :: Float -> Float -> String -> GameView -> (GameView, Int)
createParticle x y texture view = let
  currentId = view ^. currentParticleId
  particle = (x, y, texture, currentId, 255)
  newView = view & vParticles %~ (\l -> particle : l)
                 & currentParticleId %~ (+ 1)
  in (newView, currentId)

deleteParticle :: Int -> GameView -> GameView
deleteParticle id view = let
  isNotId x = x ^. _4 /= id
  in view & vParticles .~ filter isNotId (view ^. vParticles)

type Textures = Map String SDL.Texture

textureFromMap :: Textures -> String -> SDL.Texture
textureFromMap textures str = case Map.lookup str textures of
  Just texture -> texture
  Nothing -> error ("unknown texture: " ++ str)

textureNames :: [String]
textureNames = nub (getConst (moveAnim 0) ++ ["playerAtk2_0.png", "playerAtk2_1.png", "playerAtk2_2.png", "playerAtk2_3.png", "playerAtk2_4.png", "playerAtk2_5.png", "minusOne.png", "menu.jpg", "slimeHurt0.png", "slimeHurt1.png", "slimeHurt2.png", "slimeHurt3.png", "slimeIdle.png", "bgLayer0.png", "bgLayer1.png", "bgLayer2.png", "bgLayer3.png", "bgLayer4.png", "bgLayer5.png", "bgLayer6.png", "bgLayer7.png", "bgLayer8.png", "bgLayer9.png"])

-- ++ getConst attackAnim ++ ["menu.jpg"] ++ getConst slimeHurt)

data Counter = Counter
  { now :: Word64
  , prev :: Word64
  }

mkCounter :: IO Counter
mkCounter = do
  now <- SDL.Timer.getPerformanceCounter
  return (Counter now now)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "PaSe Demo" SDL.defaultWindow { SDL.windowInitialSize = V2 500 500 }
  counter <- mkCounter
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- initializeResources renderer
  appLoop renderer resources initialView initialAnims counter
  destroyResources resources
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

scrollingBg :: (Monad f, LinearTo GameView f, Set GameView f, Get GameView f) =>
  Float -> Lens' GameView Float -> Lens' GameView Bool -> f ()
scrollingBg dur counter flipped =
  linearTo counter (For dur) (To (-928))
  `sequential`
  set counter 0
  `sequential`
  ( do
    x <- get flipped
    set flipped (not x)
    )
  `sequential`
  scrollingBg dur counter flipped

menuIntro :: (Applicative f, Parallel f, LinearTo GameView f, Set GameView f) =>
  f ()
menuIntro =
  ( linearTo (vMenu . _2) (For 0.5) (To 100)
    `parallel`
    linearTo (vMenu . _3) (For 0.5) (To 255)
    )
  `sequential`
  set (vMenu . _4) True

menuOutro :: (Applicative f, Parallel f, LinearTo GameView f, Set GameView f) =>
  f ()
menuOutro =
  ( linearTo (vMenu . _2) (For 0.5) (To 80)
    `parallel`
    linearTo (vMenu . _3) (For 0.5) (To 0)
    )
  `sequential`
  set (vMenu . _4) False

moveAnim :: (Applicative f, Parallel f, LinearTo GameView f, Delay f, SetTexture GameView f) =>
  Float -> f ()
moveAnim x =
  linearTo (vPlayer . _1) (For 0.3) (To x)
  `parallel`
  sheet (vPlayer . _3) 0.1 ["playerRun0.png", "playerRun1.png", "playerRun2.png", "playerIdle.png"]

sheet :: (Applicative f, Delay f, SetTexture s f) =>
  Lens' s String -> Float -> [String] -> f ()
sheet lens time [] = pure ()
sheet lens time (frame:[]) = setTexture lens frame
sheet lens time (frame:frames) = setTexture lens frame `sequential` go frames
  where
    go (frame:[]) = delay time `sequential` setTexture lens frame
    go (frame:frames) = delay time `sequential` setTexture lens frame `sequential` go frames

attackAnim :: (Monad f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f, Parallel f, LinearTo GameView f, Create GameView f, Delete GameView f) =>
  f ()
attackAnim =
  sheet (vPlayer . _3) 0.05 ["playerAtk2_0.png", "playerAtk2_1.png", "playerAtk2_2.png", "playerAtk2_3.png"]
  `sequential`
  ( hitStop `sequential` sheet (vPlayer . _3) 0.05 ["playerAtk2_4.png", "playerAtk2_5.png", "playerIdle.png"]
    `parallel`
    slimeHurt
    )

hitStop :: (Functor f, IfThenElse f, Delay f, Get GameView f) =>
  f ()
hitStop =
  ifThenElse
    (fmap (\loc -> loc == 300) (get (vPlayer . _1)))
    (delay 0.15)
    (delay 0.05)

slimeHurtSheet :: (Applicative f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f) =>
  f ()
slimeHurtSheet = sheet (vSlime . _3) 0.1 ["slimeHurt0.png", "slimeHurt1.png", "slimeHurt2.png", "slimeHurt3.png", "slimeIdle.png"]

slimeHurt :: (Monad f, Delay f, SetTexture GameView f, Get GameView f, IfThenElse f, Delay f, Parallel f, Create GameView f, Delete GameView f, LinearTo GameView f) =>
  f ()
slimeHurt =
  ifThenElse
    (fmap (\loc -> loc == 300) (get (vPlayer . _1)))
    (delay 0.15 `sequential` (slimeHurtSheet `parallel` minusOneParticle))
    (delay 0)

minusOneParticle :: (Monad f, LinearTo GameView f, Create GameView f, Delete GameView f, Parallel f) =>
  f ()
minusOneParticle = do
  id <- create (createParticle 390 390 "minusOne.png")
  ( linearTo (vParticles . paId id . _2) (For 0.5) (To 320)
    `parallel`
    linearTo (vParticles . paId id . _5) (For 0.5) (To 0)
    )
  delete (deleteParticle id)

appLoop :: SDL.Renderer -> Textures -> GameView -> GameAnims -> Counter -> IO ()
appLoop renderer textures view anims Counter{ now, prev } = do
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
  let view1 = case (aPressed, dPressed) of
        (True, _) -> view & vPlayer . _4 .~ True
        (_, True) -> view & vPlayer . _4 .~ False
        (_, _) -> view
  let view2 = case (ePressed) of
        True -> view1 & bgCounters . moving %~ not
        _ -> view1
  let viewU = view2
  -- update animations
  let anims1 = case (aPressed, dPressed, spcPressed) of
        (True, _, _) -> anims & aPlayer %~ \l -> addIfNothing l (moveAnim ((viewU ^. vPlayer . _1) - 50))
        (_, True, _) -> anims & aPlayer %~ \l -> addIfNothing l (moveAnim ((viewU ^. vPlayer . _1) + 50))
        (_, _, True) -> anims & aPlayer %~ \l -> addIfNothing l attackAnim
        (_, _, _) -> anims
  let anims2 = case (mPressed, viewU ^. vMenu . _4) of
        (True, False) -> anims1 & aMenu %~ \l -> addIfNothing l menuIntro
        (True, True) -> anims1 & aMenu %~ \l -> addIfNothing l menuOutro
        (False, _) -> anims1
  -- update game with animations
  let (viewA, animsA) = runAllAnimations deltaTime (viewU, anims2)
  -- draw game
  drawGame renderer textures viewU
  -- loop or quit
  unless qPressed (appLoop renderer textures viewA animsA counter')

drawGame :: SDL.Renderer -> Textures -> GameView -> IO ()
drawGame renderer textures view = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer
  -- draw background
  let (bgXOffset4_A, bgXOffset4_B) = if view ^. bgCounters . flipped3
                     then (view ^. bgCounters . counter4 + 928, view ^. bgCounters . counter4)
                     else (view ^. bgCounters . counter4, view ^. bgCounters . counter4 + 928)
  for_ (reverse [8 .. 9]) $ \i -> do
    drawTexture (bgXOffset4_A) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
    drawTexture (bgXOffset4_B) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
  let (bgXOffset3_A, bgXOffset3_B) = if view ^. bgCounters . flipped2
                     then (view ^. bgCounters . counter3 + 928, view ^. bgCounters . counter3)
                     else (view ^. bgCounters . counter3, view ^. bgCounters . counter3 + 928)
  for_ (reverse [4 .. 7]) $ \i -> do
    drawTexture (bgXOffset3_A) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
    drawTexture (bgXOffset3_B) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
  let (bgXOffset2_A, bgXOffset2_B) = if view ^. bgCounters . flipped2
                     then (view ^. bgCounters . counter2 + 928, view ^. bgCounters . counter2)
                     else (view ^. bgCounters . counter2, view ^. bgCounters . counter2 + 928)
  for_ (reverse [2 .. 3]) $ \i -> do
    drawTexture (bgXOffset2_A) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
    drawTexture (bgXOffset2_B) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
  -- draw enemy
  drawTexture (view ^. vSlime . _1) (view ^. vSlime . _2) (30*2) (25*2) Nothing Nothing (view ^. vSlime . _3) renderer textures
  -- draw player
  drawTexture (view ^. vPlayer . _1) (view ^. vPlayer . _2) (50*2) (37*2) Nothing (Just (view ^. vPlayer . _4)) (view ^. vPlayer . _3) renderer textures
  -- draw foreground
  let (bgXOffset1_A, bgXOffset1_B) = if view ^. bgCounters . flipped1
                   then (view ^. bgCounters . counter1 + 928, view ^. bgCounters . counter1)
                   else (view ^. bgCounters . counter1, view ^. bgCounters . counter1 + 928)
  for_ (reverse [0 .. 1]) $ \i -> do
    drawTexture (bgXOffset1_A) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
    drawTexture (bgXOffset1_B) (-293) (928) (793) Nothing Nothing ("bgLayer" ++ show i ++ ".png") renderer textures
  -- draw particles
  for_ (view ^. vParticles) $ \particle ->
    drawTexture (particle ^. _1) (particle ^. _2) (13*2) (16*2) (Just (particle ^. _5)) Nothing (particle ^. _3) renderer textures
  -- draw menu
  drawTexture (view ^. vMenu . _1) (view ^. vMenu . _2) 300 300 (Just (view ^. vMenu . _3)) Nothing "menu.jpg" renderer textures
  SDL.present renderer

drawTexture :: Float -> Float -> Float -> Float -> Maybe Float -> Maybe Bool -> String -> SDL.Renderer -> Textures -> IO ()
drawTexture x y width height mAlpha mFlipX textureName renderer textures = let
  position = SDL.P (V2 (fromIntegral (floor x :: Int)) (fromIntegral (floor y :: Int)))
  dimension = V2 (fromIntegral (floor width :: Int)) (fromIntegral (floor height :: Int))
  texture = textureFromMap textures textureName
  in do
  case mAlpha of
    Just alpha -> do
      SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
      SDL.textureAlphaMod texture $= fromIntegral (floor alpha :: Int)
    Nothing -> return ()
  case mFlipX of
    Just flipX -> SDL.copyEx renderer texture Nothing (Just (SDL.Rectangle position dimension)) 0 Nothing (V2 flipX False)
    Nothing -> SDL.copy renderer texture Nothing (Just (SDL.Rectangle position dimension))

runAllAnimations :: Float -> (GameView, GameAnims) -> (GameView, GameAnims)
runAllAnimations delta = S.execState $ do
  S.modify (runAnimations aPlayer delta)
  S.modify (runAnimations aMenu delta)
  (view, _) <- S.get
  if view ^. bgCounters . moving
    then S.modify (runAnimations aBackground delta)
    else return ()

runAnimations :: Lens' GameAnims (Maybe (Animation GameView Identity ())) -> Float -> (GameView, GameAnims) -> (GameView, GameAnims)
runAnimations lens delta (view, anims) =
  let (view', eAnimations', _) = case anims ^. lens of
        Just anim -> runIdentity (runAnimation anim view delta)
        Nothing -> (view, Right (), Nothing)
      anims' = case eAnimations' of
        Right _ -> Nothing
        Left a -> Just a
  in (view', anims & lens .~ anims')

initializeResources :: SDL.Renderer -> IO Textures
initializeResources renderer = do
  loadedTextures <- for textureNames $ \textureName -> do
    loadTexture ("assets/" ++ textureName) renderer
  let zipped = zip textureNames loadedTextures
  return (Map.fromList zipped)

destroyResources :: Textures -> IO ()
destroyResources textures = do
  for_ textures SDL.destroyTexture

loadTexture :: FilePath -> SDL.Renderer -> IO SDL.Texture
loadTexture path renderer = do
  surface <- SDL.Image.load path
  SDL.createTextureFromSurface renderer surface

addIfNothing :: Maybe a -> a -> Maybe a
addIfNothing (Just a) _ = Just a
addIfNothing Nothing a = Just a

paId :: Int -> Lens' [(a, b, c, Int, d)] (a, b, c, Int, d)
paId i = let
  get l = case find (\x -> x ^. _4 == i) l of
    Just s -> s
    Nothing -> error ("no particle with index " ++ show i)
  set l x = case findIndex (\x -> x ^. _4 == i) l of
    Just ix -> take ix l ++ x : drop (ix+1) l
    Nothing -> error ("no particle with index " ++ show i)
  in lens get set
