{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import PaSe

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Lens.Micro hiding (set)
import Lens.Micro.TH

import Control.Monad.Identity

import System.Clock

type Dot = (Float, Float, Float, Int)

data AppState = AppState
  { _dots :: IntMap Dot
  , _index :: Int
  }

makeLenses ''AppState

data Application = Application
  { _animations :: Animation AppState Identity ()
  , _appState :: AppState
  }

makeLenses ''Application

initialApplication :: TimeSpec -> Application
initialApplication t = Application
  { _animations = mainAnimation
  , _appState = AppState
    { _dots = IntMap.empty
    , _index = 0
    }
  }

update :: Float -> Application -> IO Application
update t app = let
  (appState', animResult, _) = runIdentity $ runAnimation (app ^. animations) (app ^. appState) t
  in return $ app & appState .~ appState'
               & animations .~ case animResult of
                                      Left anims' -> anims'
                                      Right _ -> return ()

handleInput :: Event -> Application -> Application
handleInput e app = app

draw :: IORef TimeSpec -> IORef [Double] -> Application -> IO Picture
draw prevTimeRef diffsRef app = do
  -- read data
  prevTime <- readIORef prevTimeRef
  diffs <- readIORef diffsRef
  newTime <- getTime Monotonic
  -- calc diff
  let diff = ((fromIntegral (toNanoSecs newTime) - fromIntegral (toNanoSecs prevTime)) :: Double) / 1000000000
      newDiffs = addQueue 30 diff diffs
  -- write refs
  writeIORef prevTimeRef newTime
  writeIORef diffsRef newDiffs
  return $ Pictures $
    map drawDot (IntMap.elems (app ^. appState . dots)) ++
    [text ("FPS: " ++ show (fps newDiffs)) & color red & scale 0.2 0.2 & translate (-200) (200)]

drawDot :: Dot -> Picture
drawDot (x, y, alpha, _) = circleSolid 2 & Translate x y & Color (withAlpha alpha white)

main :: IO ()
main = let
  bgColor = makeColor 0 0 0 1
  sw = 500
  sh = 500
  window = InWindow "benchmarks" (sw, sh) (100, 100)
  in do
    newTime <- getTime Monotonic
    prevTimeRef <- newIORef newTime
    diffsRef <- newIORef ([] :: [Double])
    playIO window bgColor 60 (initialApplication newTime) (draw prevTimeRef diffsRef) (\e s -> return (handleInput e s)) update

createDotAnim :: (Monad f, Parallel f, LinearTo AppState f, Dots AppState f)
  => f ()
createDotAnim = do
  i <- create createDot
  ( linearTo (dots . dotId i . _1) (For 1) (To (cos (fromIntegral i) * 250))
    `parallel`
    linearTo (dots . dotId i . _2) (For 1) (To (sin (fromIntegral i) * 250))
    `parallel`
    linearTo (dots . dotId i . _3) (For 1) (To 0) )
  delete deleteDot i

mainAnimation :: (Monad f, Parallel f, Set AppState f, Delay f, LinearTo AppState f, Dots AppState f)
  => f ()
mainAnimation = do
  createDotAnim
  `parallel`
  ( do
      delay (1 / 400)
      mainAnimation
  )

dotId :: Int -> Lens' (IntMap Dot) Dot
dotId i = let
  get m = case IntMap.lookup i m of
    Just s -> s
    Nothing -> error ("no particle with index " ++ show i)
  set m x = IntMap.insert i x m
  in lens get set

createDot :: AppState -> (AppState, Int)
createDot app = let
  currentId = app ^. index
  dot = (0, 0, 1, currentId)
  app' = app & dots %~ (\m -> IntMap.insert currentId dot m)
             & index %~ (+ 1)
  in (app', currentId)

deleteDot :: Int -> AppState -> AppState
deleteDot id app = app & dots %~ IntMap.delete id

class Dots s f where
  create :: (s -> (s, Int)) -> f Int
  delete :: (Int -> s -> s) -> Int -> f ()

instance (Monad m) => Dots s (Animation s m) where
  create f = Animation $ \s delta -> let
    (s', i) = (f s)
    in return (s', Right i, Just delta)
  delete f i = Animation $ \s delta -> let
    s' = f i s
    in return (s', Right (), Just delta)

addQueue :: Int -> a -> [a] -> [a]
addQueue i a l = take i (a:l)

avg :: (Fractional a) => [a] -> a
avg xs = sum xs / fromIntegral (length xs)

fps :: [Double] -> Int
fps diffs = floor (1 / avg diffs)
