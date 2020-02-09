{-# LANGUAGE TemplateHaskell #-}

module View where

import Sprite
import Textures

import qualified SDL
import Data.Foldable (for_)

import Lens.Micro
import Lens.Micro.TH

data GameView = GameView
  { _player :: Sprite
  , _slime :: Sprite
  , _particles :: [(Sprite, Int)]
  , _bgSection0A :: Sprite -- front
  , _bgSection0B :: Sprite
  , _bgSection1A :: Sprite
  , _bgSection1B :: Sprite
  , _bgSection2A :: Sprite
  , _bgSection2B :: Sprite
  , _bgSection3A :: Sprite
  , _bgSection3B :: Sprite -- back
  , _menu :: Sprite
  , _bgMoving :: Bool
  , _currentParticleId :: Int
  }

initialPlayer :: Sprite
initialPlayer = Sprite
  { _x = 50
  , _y = 365
  , _width = 50*2
  , _height = 37*2
  , _alpha = 255
  , _flippedX = False
  , _texture = "playerIdle.png"
  }

initialSlime :: Sprite
initialSlime = Sprite
  { _x = 370
  , _y = 390
  , _width = 30*2
  , _height = 25*2
  , _alpha = 255
  , _flippedX = False
  , _texture = "slimeIdle.png"
  }

data Section = SectionA | SectionB
  deriving (Eq)

initialBgSection :: Int -> Section -> Sprite
initialBgSection i section = Sprite
  { _x = if section == SectionA then 0 else 928
  , _y = -293
  , _width = 928
  , _height = 793
  , _alpha = 255
  , _flippedX = False
  , _texture = "bgSection" ++ show i ++ ".png"
  }

initialMenu :: Sprite
initialMenu = Sprite
  { _x = 56
  , _y = 40
  , _width = 388
  , _height = 162
  , _alpha = 0
  , _flippedX = False
  , _texture = "menu.png"
  }

initialView :: GameView
initialView = GameView
  { _player = initialPlayer
  , _slime = initialSlime
  , _particles = []
  , _bgSection0A = initialBgSection 0 SectionA
  , _bgSection0B = initialBgSection 0 SectionB
  , _bgSection1A = initialBgSection 1 SectionA
  , _bgSection1B = initialBgSection 1 SectionB
  , _bgSection2A = initialBgSection 2 SectionA
  , _bgSection2B = initialBgSection 2 SectionB
  , _bgSection3A = initialBgSection 3 SectionA
  , _bgSection3B = initialBgSection 3 SectionB
  , _menu = initialMenu
  , _bgMoving = False
  , _currentParticleId = 0
  }

makeLenses ''GameView

drawView :: GameView -> SDL.Renderer -> Textures -> IO ()
drawView view renderer textures = do
  drawSprite (view ^. bgSection3A) renderer textures
  drawSprite (view ^. bgSection3B) renderer textures
  drawSprite (view ^. bgSection2A) renderer textures
  drawSprite (view ^. bgSection2B) renderer textures
  drawSprite (view ^. bgSection1A) renderer textures
  drawSprite (view ^. bgSection1B) renderer textures
  drawSprite (view ^. slime) renderer textures
  drawSprite (view ^. player) renderer textures
  for_ (view ^. particles) $ \(particle, _) ->
    drawSprite particle renderer textures
  drawSprite (view ^. bgSection0A) renderer textures
  drawSprite (view ^. bgSection0B) renderer textures
  drawSprite (view ^. menu) renderer textures
