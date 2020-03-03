{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import PaSe

import Data.List (find, findIndex)

import Graphics.Gloss
import Graphics.Gloss.Export.Gif
import Graphics.Gloss.Interface.Pure.Game

import Lens.Micro hiding (set)
import Lens.Micro.TH

import Control.Monad.Identity

type Dot = (Float, Float, Float, Int)

data AppState = AppState
  { _dots :: [Dot]
  , _index :: Int
  }

makeLenses ''AppState

data Application = Application
  { _animations :: Animation AppState Identity ()
  , _appState :: AppState
  }

makeLenses ''Application

initialApplication :: Application
initialApplication = Application
  { _animations = mainAnimation
  , _appState = AppState
    { _dots = []
    , _index = 0
    }
  }

update :: Float -> Application -> Application
update t app = let
  (appState', animResult, _) = runIdentity $ runAnimation (app ^. animations) (app ^. appState) t
  in case animResult of
    Left anims' -> app & appState .~ appState'
                       & animations .~ anims'
    Right _ -> app & appState .~ appState'
                   & animations .~ return ()

handleInput :: Event -> Application -> Application
handleInput e app = app

draw :: Application -> Picture
draw app = Pictures (map drawDot (app ^. appState . dots))

drawDot :: Dot -> Picture
drawDot (x, y, alpha, _) = circleSolid 2 & Translate x y & Color (withAlpha alpha white)

main :: IO ()
main = let
  bgColor = makeColor 0 0 0 1
  sw = 500
  sh = 500
  window = InWindow "benchmarks" (sw, sh) (100, 100)
  in play window bgColor 60 initialApplication draw handleInput update

createDotAnim :: (Monad f, Parallel f, LinearTo AppState f, Dots AppState f)
  => f ()
createDotAnim = do
  i <- create createDot
  ( linearTo (dots . dotId i . _1) (For 3) (To (cos (fromIntegral i) * 250))
    `parallel`
    linearTo (dots . dotId i . _2) (For 3) (To (sin (fromIntegral i) * 250))
    `parallel`
    linearTo (dots . dotId i . _3) (For 3) (To 0) )
  delete deleteDot i

mainAnimation :: (Monad f, Parallel f, Set AppState f, Delay f, LinearTo AppState f, Dots AppState f)
  => f ()
mainAnimation = do
  createDotAnim
  `parallel`
  ( do
      delay 0.01
      mainAnimation
  )

dotId :: Int -> Lens' [Dot] Dot
dotId i = let
  get l = case find (\x -> x ^. _4 == i) l of
    Just s -> s
    Nothing -> error ("no particle with index " ++ show i)
  set l x = case findIndex (\x -> x ^. _4 == i) l of
    Just ix -> take ix l ++ x : drop (ix+1) l
    Nothing -> error ("no particle with index " ++ show i)
  in lens get set

createDot :: AppState -> (AppState, Int)
createDot app = let
  currentId = app ^. index
  dot = (0, 0, 1, currentId)
  app' = app & dots %~ (\l -> dot : l)
             & index %~ (+ 1)
  in (app', currentId)

deleteDot :: Int -> AppState -> AppState
deleteDot id app = let
  isNotId x = x ^. _4 /= id
  in app & dots %~ filter isNotId

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

