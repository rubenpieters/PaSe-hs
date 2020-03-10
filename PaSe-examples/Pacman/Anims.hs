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
  { _pacmanMoveAnims :: Maybe (Animation GameView Identity ())
  , _ghostMoveAnims :: Maybe (Animation GameView Identity ())
  , _particleAnims :: Maybe (Animation GameView Identity ())
  , _readyAnims :: Maybe (Animation GameView Identity ())
  , _deathAnims :: Maybe (Animation GameView Identity ())
  }

makeLenses ''GameAnims

initialAnims :: GameAnims
initialAnims = GameAnims
  { _pacmanMoveAnims = Nothing
  , _ghostMoveAnims = Nothing
  , _particleAnims = Nothing
  , _readyAnims = Just (readyAnim 300 300)
  , _deathAnims = Nothing
  }

runAllAnimations :: Float -> (GameView, GameAnims) -> (GameView, GameAnims)
runAllAnimations delta = S.execState $ do
  S.modify (continueI pacmanMoveAnims delta)
  S.modify (continueI ghostMoveAnims delta)
  S.modify (continueI particleAnims delta)
  S.modify (continueI readyAnims delta)
  S.modify (continueI deathAnims delta)
