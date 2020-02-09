module Textures where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable (for_)
import Data.Traversable (for)

import qualified SDL
import qualified SDL.Image

type Textures = Map String SDL.Texture

textureFromMap :: Textures -> String -> SDL.Texture
textureFromMap textures str = case Map.lookup str textures of
  Just texture -> texture
  Nothing -> error ("unknown texture: " ++ str)

destroyTextures :: Textures -> IO ()
destroyTextures textures = do
  for_ textures SDL.destroyTexture

loadTextures :: [String] -> SDL.Renderer -> IO Textures
loadTextures textureNames renderer = do
  loadedTextures <- for textureNames $ \textureName -> do
    loadTexture ("assets/" ++ textureName) renderer
  let zipped = zip textureNames loadedTextures
  return (Map.fromList zipped)

loadTexture :: FilePath -> SDL.Renderer -> IO SDL.Texture
loadTexture path renderer = do
  surface <- SDL.Image.load path
  SDL.createTextureFromSurface renderer surface
