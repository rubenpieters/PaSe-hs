module PaSe.Arrow.Const where

import Control.Arrow
import Control.Category

newtype ConstA a i o = ConstA { getConstA :: a }
  deriving (Eq, Show, Ord)

instance (Monoid a) => Category (ConstA a) where
  id = ConstA mempty
  (ConstA a1) . (ConstA a2) = ConstA (a1 <> a2)

instance (Monoid a) => Arrow (ConstA a) where
  arr _ = ConstA mempty
  first (ConstA a) = ConstA a
