module Textures where

import PaSe (Texture(..))

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable (for_)
import Data.Traversable (for)

import qualified SDL
import qualified SDL.Image

type Textures = Map String SDL.Texture

textureFromMap :: Textures -> Texture -> SDL.Texture
textureFromMap textures (Texture textureName) = case Map.lookup textureName textures of
  Just texture -> texture
  Nothing -> error ("unknown texture: " ++ textureName)

destroyTextures :: Textures -> IO ()
destroyTextures textures = do
  for_ textures SDL.destroyTexture

loadTextures :: [Texture] -> SDL.Renderer -> IO Textures
loadTextures textures renderer = do
  loadedTextures <- for textures $ \(Texture textureName) -> do
    loadTexture ("assets/" ++ textureName) renderer
  let zipped = zip (map getTexture textures) loadedTextures
  return (Map.fromList zipped)

loadTexture :: FilePath -> SDL.Renderer -> IO SDL.Texture
loadTexture path renderer = do
  surface <- SDL.Image.load path
  SDL.createTextureFromSurface renderer surface
