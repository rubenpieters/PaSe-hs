{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import PaSe

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Lens.Micro hiding (set)
import Lens.Micro.TH

import Control.Monad.Identity

data AppState = AppState
  { _stageText :: String
  , _mail :: (Float, Float, Float)
  , _chat :: (Float, Float, Float)
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
    { _stageText = ""
    , _mail = (0, 0, 0)
    , _chat = (0, 0, 0)
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

alicePic :: Picture
alicePic = Pictures
  [ ThickCircle 30 5
  , Polygon [(-2.5, -30), (-2.5, -60), (2.5, -60), (2.5, -30)]
  , Text "Alice" & Scale 0.3 0.3 & Translate 0 (-100)
  ] & Color red & Translate (-150) 30

alicePos :: (Float, Float)
alicePos = (-150, 30)

bobPic :: Picture
bobPic = Pictures
  [ ThickCircle 30 5
  , Polygon [(-2.5, -30), (-2.5, -60), (2.5, -60), (2.5, -30)]
  , Text "Bob" & Scale 0.3 0.3 & Translate 0 (-100)
  ] & Color blue & Translate 150 30

bobPos :: (Float, Float)
bobPos = (150, 30)

mailPic :: Picture
mailPic = Pictures
  [ rectangleWire 6 6
  , line [(3, 3), (-3, 3), (0, 0), (3, 3)]
  ] & Scale 5 5

chatPic :: Picture
chatPic = Pictures
  [ rectangleSolid 6 2
  , Polygon [(1, -1), (2, -1), (3, -2)]
  ] & Scale 20 20

draw :: Application -> Picture
draw app = let
  stageTextDrawn = Text (app ^. appState . stageText) & Scale 0.4 0.4 & Translate (-130) 130 & Color white
  mailDrawn = mailPic & Translate (app ^. appState . mail . _1) (app ^. appState . mail . _2) & Color (withAlpha (app ^. appState . mail . _3) white)
  chatDrawn = chatPic & Translate (app ^. appState . chat . _1) (app ^. appState . chat . _2) & Color (withAlpha (app ^. appState . chat . _3) white)
  in Pictures [alicePic, bobPic, stageTextDrawn, mailDrawn, chatDrawn]

main :: IO ()
main = let
  bgColor = makeColor 0 0 0 1
  sw = 500
  sh = 500
  window = InWindow "animation-dsl" (sw, sh) (100, 100)
  in play window bgColor 60 initialApplication draw handleInput update

sendMail :: (Monad f, Parallel f, Set AppState f, LinearTo AppState f) => (Float, Float) -> (Float, Float) -> f ()
sendMail (fromX, fromY) (toX, toY) = do
  set (mail . _1) (fromX + 20)
  set (mail . _2) (fromY - 15)
  ( linearTo (mail . _2) (For 1) (To fromY)
    `parallel`
    linearTo (mail . _3) (For 1) (To 1) )
  linearTo (mail . _1) (For 2) (To (toX - 20))
  ( linearTo (mail . _2) (For 1) (To (toY - 15))
    `parallel`
    linearTo (mail . _3) (For 1) (To 0) )

communicate :: (Monad f, Parallel f, Set AppState f, Delay f, LinearTo AppState f) => f ()
communicate = do
  set (chat . _1) (70)
  set (chat . _2) (80)
  ( linearTo (chat . _2) (For 1) (To 100)
    `parallel`
    linearTo (chat . _3) (For 1) (To 1) )
  delay 1
  ( linearTo (chat . _2) (For 1) (To (80))
    `parallel`
    linearTo (chat . _3) (For 1) (To 0) )

mainAnimation :: (Monad f, Parallel f, Set AppState f, Delay f, LinearTo AppState f) => f ()
mainAnimation = do
  set (stageText) "Drafting"
  delay 1
  set (stageText) "Submission"
  sendMail alicePos bobPos
  delay 1
  set (stageText) "Feedback"
  communicate
  delay 1
  set (stageText) "Correction"
  sendMail alicePos bobPos

