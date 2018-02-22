module Hasami.Renderer
  ( Renderer(..)
  , Texture(..)
  , Buffer(..)
  , Shader(..)
  , TextureUnit(..)
  , UniformValue(..)
  )
where

import Foreign
import Data.Vect
import Control.Monad.IO.Class
import qualified Data.Vector.Storable as V

-- | Texture units
data TextureUnit = Texture0

-- | Uniform types
data UniformValue = UniformFloat Float | UniformTexture TextureUnit | UniformMatrix Mat4

-- | Renderer interface
data Renderer = Renderer
  { swapBuffers :: forall m. MonadIO m => m ()
  , renderClear :: forall m. MonadIO m => Float -> Float -> Float -> Float -> m ()
  , loadShader :: forall m. MonadIO m => FilePath -> m (Shader)
  , loadTexture :: forall m. MonadIO m => FilePath -> m (Texture)
  , createBuffer :: forall a. Storable a => V.Vector a -> Maybe Int32 -> Maybe Int32 -> Buffer a
  }

-- | Texture interface
data Texture = Texture
  { bindTexture :: forall m. MonadIO m => TextureUnit -> m ()
  }

-- | Buffer interface
data Buffer a = Buffer
  { drawBuffer :: forall m. MonadIO m => m ()
  }

-- | Shader interface
data Shader = Shader
  { bindShader :: forall m. MonadIO m => m ()
  , unbindShader :: forall m. MonadIO m => m ()
  , setUniform :: forall m. MonadIO m => String -> UniformValue -> m ()
  }
