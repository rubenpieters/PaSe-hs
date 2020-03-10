module Field where

import Types

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function ((&))

type Field = Map (Int, Int) TileValue

tileValue :: Field -> (Int, Int) -> Maybe TileValue
tileValue field loc = Map.lookup loc field

locInDir :: (Int, Int) -> Dir -> (Int, Int)
locInDir (x, y) DirUp = (x, y - 1)
locInDir (x, y) DirDown = (x, y + 1)
locInDir (x, y) DirLeft = (x - 1, y)
locInDir (x, y) DirRight = (x + 1, y)

tileInDir :: (Int, Int) -> Dir -> Field -> Maybe TileValue
tileInDir loc dir field = tileValue field (locInDir loc dir)

nonWallDirs :: Field -> (Int, Int) -> [Dir]
nonWallDirs field loc = [DirUp, DirDown, DirLeft, DirRight]
  & map (\d -> (d, tileInDir loc d field))
  & filter (\(_, val) -> val /= Just Wall)
  & map fst
