{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Anims where

import PaSe

import View (GameView)
import qualified View as V
import AnimDefs

import Control.Monad.Identity
import qualified Control.Monad.State as S

import Lens.Micro
import Lens.Micro.TH

data GameAnims = GameAnims
  { _pacman :: Maybe (Animation GameView Identity ())
  , _pacmanM :: Maybe (Animation GameView Identity ())
  , _particleAnims :: Maybe (Animation GameView Identity ())
  }

makeLenses ''GameAnims

initialAnims :: GameAnims
initialAnims = GameAnims
  { _pacman = Nothing
  , _pacmanM = Nothing
  , _particleAnims = Just (readyAnim 300 300)
  }

runAllAnimations :: Float -> (GameView, GameAnims) -> (GameView, GameAnims)
runAllAnimations delta = S.execState $ do
  S.modify (continueI pacman delta)
  S.modify (continueI pacmanM delta)
  S.modify (continueI particleAnims delta)
