module Types where

data Dir = DirUp | DirDown | DirLeft | DirRight
  deriving (Eq, Show, Ord)

data TileValue = Dot | Wall
  deriving (Eq, Show, Ord)



