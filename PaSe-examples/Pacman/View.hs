{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module View where

import PaSe (Texture(..))

import Types
import Sprite
import Textures
import Field

import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified SDL

import Lens.Micro
import Lens.Micro.TH

data PacmanExtra = PacmanExtra
  { _moveDir :: Dir
  , _alive :: Bool
  }

makeLenses ''PacmanExtra

data GameView = GameView
  { _pacmanSprite :: Sprite
  , _pacmanExtra :: PacmanExtra
  , _ghostSprite :: Sprite
  , _field :: Field
  , _particles :: [(Sprite, Int)]
  , _currentParticleId :: Int
  }

tileLoc :: Sprite -> (Int, Int)
tileLoc Sprite{ _x, _y } = (floor (_x / 30), floor (_y / 30))

makeLenses ''GameView

initialPacman :: Sprite
initialPacman = Sprite
  { _x = 30
  , _y = 30
  , _width = 30
  , _height = 30
  , _alpha = 255
  , _flippedX = False
  , _flippedY = False
  , _rotation = 0
  , _texture = Texture "pacman0.png"
  }

initialPacmanExtra :: PacmanExtra
initialPacmanExtra = PacmanExtra
  { _moveDir = DirRight
  , _alive = True
  }

initialGhost :: Sprite
initialGhost = Sprite
  { _x = 540
  , _y = 540
  , _width = 30
  , _height = 30
  , _alpha = 255
  , _flippedX = False
  , _flippedY = False
  , _rotation = 0
  , _texture = Texture "ghost0.png"
  }

initialField :: Field
initialField = parseField
  [ "wwwwwwwwwwwwwwwwwwww"
  , "w........ww........w"
  , "w...ww...ww....ww..w"
  , "w...ww...ww....ww..w"
  , "w..................w"
  , "wwww..ww....ww..wwww"
  , "wwww..ww....ww..wwww"
  , "w.....ww....ww.....w"
  , "w.....ww....ww.....w"
  , "w..................w"
  , "w.....ww....ww.....w"
  , "wwww..ww....ww..wwww"
  , "wwww..ww....ww..wwww"
  , "w..................w"
  , "w...w.........w....w"
  , "w...w.wwwwwww.w....w"
  , "w...w....ww...w....w"
  , "w..wwwww.ww.wwww...w"
  , "w..................w"
  , "wwwwwwwwwwwwwwwwwwww"
  ]

initialView :: GameView
initialView = GameView
  { _pacmanSprite = initialPacman
  , _pacmanExtra = initialPacmanExtra
  , _ghostSprite = initialGhost
  , _field = initialField
  , _particles = []
  , _currentParticleId = 0
  }

tileSprite :: (Int, Int) -> TileValue -> Sprite
tileSprite (x, y) v = Sprite
  { _x = fromIntegral (x * 30 + tileOffset v)
  , _y = fromIntegral (y * 30 + tileOffset v)
  , _width = fromIntegral (tileSize v)
  , _height = fromIntegral (tileSize v)
  , _alpha = 255
  , _flippedX = False
  , _flippedY = False
  , _rotation = 0
  , _texture = tileTexture v
  }

tileSize :: TileValue -> Int
tileSize Dot = 10
tileSize Wall = 20

tileOffset :: TileValue -> Int
tileOffset v = floor (fromIntegral (30 - tileSize v) / 2 :: Float)

tileTexture :: TileValue -> Texture
tileTexture Dot = Texture "dot1.png"
tileTexture Wall = Texture "wall.png"

drawView :: GameView -> SDL.Renderer -> Textures -> IO ()
drawView view renderer textures = do
  for_ (view ^. field & Map.assocs) $ \(k, v) -> do
    drawSprite (tileSprite k v) renderer textures
  drawSprite (view ^. ghostSprite) renderer textures
  drawSprite (view ^. pacmanSprite) renderer textures
  for_ (view ^. particles) $ \(sprite, id) -> do
    drawSprite sprite renderer textures

