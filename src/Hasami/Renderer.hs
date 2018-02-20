module Hasami.Renderer where

import SDL (($=))
import qualified SDL
import Control.Monad (unless)
import Control.Monad.State
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vect
--import Paths_hasami
import Foreign
import Graphics.GLUtil (readTexture, texture2DWrap)

class Mat a where
  toGLMat :: a -> IO (GL.GLmatrix Float)
  
instance Mat Mat4 where
  toGLMat (Mat4 (Vec4 a b c d) (Vec4 e f g h) (Vec4 i j k l) (Vec4 m n o p)) = do
    let mat = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]
    GL.newMatrix GL.ColumnMajor mat :: IO (GL.GLmatrix Float)

instance Mat Proj4 where
  toGLMat proj = toGLMat . fromProjective $ proj

unsafeWith :: Storable a => a -> (Ptr b -> IO c) -> IO c
unsafeWith v act = alloca $ \a -> poke a v >> act (castPtr a)

-- | RenderState monad
newtype RS a = RS {unpackRS :: StateT RenderState IO a} deriving (Monad, Applicative, Functor, MonadIO, MonadState RenderState)

-- | Function to run a render state
runRS :: RS a -> RenderState -> IO (a, RenderState)
runRS a = runStateT (unpackRS a)

-- | Renderer state
data RenderState = RenderState
  { _swapBuffers :: IO ()
  }
makeLenses ''RenderState

-- | Initial renderer state
initialRenderState :: RenderState
initialRenderState = RenderState
  { _swapBuffers = undefined
  }

-- | Initialise renderer
initRenderer :: SDL.Window -> IO RenderState
initRenderer win = do
  pure $ RenderState
    { _swapBuffers = SDL.glSwapWindow win
    }

-- Attribute locations
posAttrib :: GL.AttribLocation
uvsAttrib :: GL.AttribLocation
posAttrib = GL.AttribLocation 0
uvsAttrib = GL.AttribLocation 1

-- | Bind our standard attrib locations
initAttribs :: GL.Program -> IO ()
initAttribs prog = do
  GL.attribLocation prog "in_pos" $= posAttrib
  GL.attribLocation prog "in_uvs" $= uvsAttrib

-- | Load shader
loadShader :: FilePath -> IO (GL.Program)
loadShader path = do
  source <- readFile path
  let vsSource = foldr (++) "" ["#version 330\n", "#define BUILDING_VERTEX_SHADER\n", source]
  let fsSource = foldr (++) "" ["#version 330\n", "#define BUILDING_FRAGMENT_SHADER\n", source]

  vs <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vs $= (GL.packUtf8 vsSource)
  GL.compileShader vs
  vsOk <- GL.get $ GL.compileStatus vs
  unless vsOk $ do
    putStrLn $ "Error in vertex shader"
    slog <- GL.get $ GL.shaderInfoLog vs
    putStrLn slog

  fs <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS fs $= (GL.packUtf8 fsSource)
  GL.compileShader fs
  fsOk <- GL.get $ GL.compileStatus fs
  unless fsOk $ do
    putStrLn $ "Error in fragment shader"
    slog <- GL.get $ GL.shaderInfoLog fs
    putStrLn slog

  prog <- GL.createProgram
  GL.attachShader prog vs
  GL.attachShader prog fs
  initAttribs prog
  GL.linkProgram prog
  linkOK <- GL.get $ GL.linkStatus prog
  GL.validateProgram prog
  status <- GL.get $ GL.validateStatus prog
  unless (linkOK && status) $ do
    putStrLn "Error linking program"
    plog <- GL.get $ GL.programInfoLog prog
    putStrLn plog

  pure prog

-- | Load texture
loadTex :: FilePath -> IO GL.TextureObject
loadTex path = do
  t <- either error id <$> readTexture path
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  return t
