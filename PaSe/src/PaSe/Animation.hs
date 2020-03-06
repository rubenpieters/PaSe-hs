{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module PaSe.Animation where

import PaSe.Types
import PaSe.Mtl

import Control.Monad (ap, liftM)
import Control.Monad.Identity

import Lens.Micro hiding (set)

newtype Animation s m a = Animation {
  runAnimation ::
    s -> -- previous state
    Float -> -- time delta
    m -- result is wrapped in m
      ( s -- next state
      , Either
        (Animation s m a) -- animation remainder
        a -- animation result
      , Maybe Float -- remaining time delta
      )
  }

instance (Monad m) => Functor (Animation obj m) where
  fmap = liftM

instance (Monad m) => Applicative (Animation obj m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (Animation obj m) where
  return a = Animation $ \obj t -> return (obj, Right a, Just t)
  (Animation f) >>= k = Animation $ \obj t -> do
    (newObj, animResult, mRemainingDelta) <- f obj t
    case (animResult, mRemainingDelta) of
      (Left anim, Nothing) ->
        return (newObj, Left (anim >>= k), Nothing)
      (Right a, Nothing) ->
        return (newObj, Left (k a), Nothing)
      (Left anim, Just remainingDelta) ->
        runAnimation (anim >>= k) newObj remainingDelta
      (Right a, Just remainingDelta) ->
        runAnimation (k a) newObj remainingDelta

instance (Monad m) => Parallel (Animation obj m) where
  liftP2 combine (Animation f1) (Animation f2) =
     Animation $ \s0 t -> do
      (s1, remAnim1, mRem1) <- f1 s0 t
      (s2, remAnim2, mRem2) <- f2 s1 t
      let newRem = case (mRem1, mRem2) of
            (Nothing, _) -> Nothing
            (_, Nothing) -> Nothing
            (Just rem1, Just rem2) -> Just (min rem1 rem2)
      let newAnim = case (remAnim1, remAnim2) of
            (Right a, Right b) ->
              Right (combine a b)
            (Left aniA, Right b) ->
              Left (fmap (\a -> combine a b) aniA)
            (Right a, Left aniB) ->
              Left (fmap (\b -> combine a b) aniB)
            (Left aniA, Left aniB) ->
              Left (liftP2 combine aniA aniB)
      case (newRem, newAnim) of
        (Just rem, Left anim) -> runAnimation anim s2 rem
        (_, _) -> return (s2, newAnim, newRem)

instance (Applicative m) => LinearTo obj (Animation obj m) where
  linearTo traversal (For duration) (To target) =
    Animation $ \obj t -> let
    -- construct new object state
    newObj = obj & traversal %~ updateValue t duration target
    -- calculate remaining duration of this basic animation
    newDuration = duration - t
    -- create remainder animation / time delta
    (remainingAnim, remainingDelta) =
      if newDuration > 0
      then ( Left (linearTo traversal (For newDuration) (To target))
           , Nothing
           )
      else (Right (), Just (-newDuration))
    in pure (newObj, remainingAnim, remainingDelta)

updateValue ::
  Float -> -- time delta
  Float -> -- duration
  Float -> -- target value
  Float -> -- current value
  Float -- new value
updateValue t duration target current = let
  speed = (target - current) * t / duration
  newValue = current + speed
  in if target > current
    then min target newValue
    else max target newValue

instance (Applicative m) => Set s (Animation s m) where
  set lens a = Animation $ \s t -> let
    newObj = s & lens .~ a
    in pure (newObj, Right (), Just t)

instance (Applicative m) => Get s (Animation s m) where
  get lens = Animation $ \s t -> let
    a = s ^. lens
    in pure (s, Right a, Just t)

instance (Applicative m) => Delay (Animation s m) where
  delay duration = Animation $ \s t -> let
    newDuration = duration - t
    (remainingAnim, remainingDelta) =
      if newDuration > 0
      then ( Left (delay newDuration)
           , Nothing
           )
      else (Right (), Just (-newDuration))
    in pure (s, remainingAnim, remainingDelta)

instance (Monad m) => IfThenElse (Animation s m) where
  ifThenElse fBool fThen fElse = do
    b <- fBool
    if b then fThen else fElse

instance (Applicative m) => SetTexture s (Animation s m) where
  setTexture = set

continue :: (Monad m) => Lens' anims (Maybe (Animation view m ())) -> Float -> (view, anims) -> m (view, anims)
continue lens delta (view, anims) = case anims ^. lens of
    Just anim -> do
      (view', eResult, _) <- runAnimation anim view delta
      case eResult of
        Left anim' -> return (view', anims & lens .~ Just anim')
        Right _ -> return (view', anims & lens .~ Nothing)
    Nothing -> return (view, anims & lens .~ Nothing)

continueI :: Lens' anims (Maybe (Animation view Identity ())) -> Float -> (view, anims) -> (view, anims)
continueI lens delta s = runIdentity (continue lens delta s)
