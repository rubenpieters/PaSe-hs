module PaSe.Types where

newtype Duration = For { getDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup Duration where
  (<>) = mappend

instance Monoid Duration where
  mempty = For 0
  mappend (For a) (For b) = For (a + b)

newtype MaxDuration = MaxFor { getMaxDuration :: Float }
  deriving (Ord, Eq, Show)

instance Semigroup MaxDuration where
  (<>) = mappend

instance Monoid MaxDuration where
  mempty = MaxFor 0
  mappend (MaxFor a) (MaxFor b) = MaxFor (a + b)

newtype Target = To { getTarget :: Float }
  deriving (Ord, Eq, Show)

newtype Texture = Texture { getTexture :: String }
  deriving (Ord, Eq, Show)
