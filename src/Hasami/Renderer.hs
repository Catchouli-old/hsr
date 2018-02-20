module Hasami.Renderer
where

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
import qualified Data.Vector.Storable as V

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
  { _rendererWindow :: SDL.Window
  }
makeLenses ''RenderState

-- | Swap buffers
swapBuffers :: (MonadState RenderState m, MonadIO m) => m ()
swapBuffers = do
  win <- use rendererWindow
  liftIO $ SDL.glSwapWindow win

renderClear :: (MonadIO m) => GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> m ()
renderClear r g b a = liftIO $ do
  GL.clearColor $= GL.Color4 r g b a
  GL.clear [GL.ColorBuffer]

-- | Initialise renderer
initRenderer :: SDL.Window -> IO RenderState
initRenderer win = do
  pure $ RenderState
    { _rendererWindow = win
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

-- | Shader
data Shader = Shader
  { programId :: GL.Program
  }

-- | Bind shader
bindShader :: MonadIO m => Shader -> m ()
bindShader (Shader prog) = GL.currentProgram $= Just prog

-- | Unbind shader
unbindShader :: MonadIO m => m ()
unbindShader = GL.currentProgram $= Nothing

-- | Set uniform
setUniform :: (GL.Uniform a, MonadIO m) => Shader -> String -> a -> m ()
setUniform (Shader prog) name val = do
  GL.currentProgram $= Just prog
  loc <- GL.get (GL.uniformLocation prog name)
  GL.uniform loc $= val

-- | Load shader
loadShader :: FilePath -> IO Shader
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

  pure $ Shader { programId = prog
                }

-- | Texture type
data Texture = Texture
  { textureId :: GL.TextureObject
  }

-- | Bind texture to unit in shader
bindTexture :: MonadIO m => Shader -> String -> GL.GLuint -> Texture -> m ()
bindTexture shader uniform unit (Texture texid) = do
  setUniform shader uniform (GL.TextureUnit unit)
  GL.activeTexture $= GL.TextureUnit unit
  GL.textureBinding GL.Texture2D $= Just texid

-- | Load texture
loadTex :: FilePath -> IO Texture
loadTex path = do
  t <- either error id <$> readTexture path
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  return $ Texture t

-- | Buffer
data Buffer a = Buffer
  { bufferData :: V.Vector a
  , posDims :: Maybe Int32
  , uvsDims :: Maybe Int32
  }

-- | Draw buffer
drawBuffer :: Storable a => Buffer a -> IO ()
drawBuffer (Buffer vec pos uvs) = do
  let undefA = V.head vec
  let moz = maybe 0 id
  let stride = fromIntegral (sizeOf undefA) * (moz pos + moz uvs) :: Int32
  let posOffset = 0
  let uvsOffset = fromIntegral (sizeOf undefA) * fromIntegral (moz pos)
  let elementCount = fromIntegral (sizeOf undefA * V.length vec `div` fromIntegral stride) :: Int32

  case pos of
     Nothing -> GL.vertexAttribArray posAttrib $= GL.Disabled
     Just i -> do
       GL.vertexAttribArray posAttrib $= GL.Enabled
       V.unsafeWith vec $ \ptr ->
         GL.vertexAttribPointer posAttrib $= (GL.ToFloat, GL.VertexArrayDescriptor i GL.Float stride (plusPtr ptr posOffset))

  case uvs of
    Nothing -> GL.vertexAttribArray uvsAttrib $= GL.Disabled
    Just i -> do
      GL.vertexAttribArray uvsAttrib $= GL.Enabled
      V.unsafeWith vec $ \ptr ->
        GL.vertexAttribPointer uvsAttrib $= (GL.ToFloat, GL.VertexArrayDescriptor i GL.Float stride (plusPtr ptr uvsOffset))

  GL.drawArrays GL.Triangles 0 elementCount

  GL.vertexAttribArray posAttrib $= GL.Disabled
  GL.vertexAttribArray uvsAttrib $= GL.Disabled
