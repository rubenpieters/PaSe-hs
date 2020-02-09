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
  { _player :: Maybe (Animation GameView Identity ())
  , _slime :: Maybe (Animation GameView Identity ())
  , _particles :: Maybe (Animation GameView Identity ())
  , _background :: Maybe (Animation GameView Identity ())
  , _menu :: Maybe (Animation GameView Identity ())
  }

makeLenses ''GameAnims

initialAnims :: GameAnims
initialAnims = GameAnims
  { _player = Nothing
  , _slime = Nothing
  , _particles = Nothing
  , _background = Just parallax
  , _menu = Just menuIntro
  }

runAnimations :: Lens' GameAnims (Maybe (Animation GameView Identity ())) -> Float -> (GameView, GameAnims) -> (GameView, GameAnims)
runAnimations lens delta (view, anims) =
  let (view', eAnimations', _) = case anims ^. lens of
        Just anim -> runIdentity (runAnimation anim view delta)
        Nothing -> (view, Right (), Nothing)
      anims' = case eAnimations' of
        Right _ -> Nothing
        Left a -> Just a
  in (view', anims & lens .~ anims')

runAllAnimations :: Float -> (GameView, GameAnims) -> (GameView, GameAnims)
runAllAnimations delta = S.execState $ do
  S.modify (runAnimations player delta)
  S.modify (runAnimations menu delta)
  (view, _) <- S.get
  if view ^. V.bgMoving
    then S.modify (runAnimations background delta)
    else return ()
