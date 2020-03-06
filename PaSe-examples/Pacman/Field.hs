module Field where

import Types

import Data.Map (Map)
import qualified Data.Map as Map

type Field = Map (Int, Int) TileValue

tileValue :: Field -> (Int, Int) -> Maybe TileValue
tileValue field loc = Map.lookup loc field

tileInDir :: (Int, Int) -> Dir -> Field -> Maybe TileValue
tileInDir (x, y) DirUp field = tileValue field (x, y - 1)
tileInDir (x, y) DirDown field = tileValue field (x, y + 1)
tileInDir (x, y) DirLeft field = tileValue field (x - 1, y)
tileInDir (x, y) DirRight field = tileValue field (x + 1, y)
