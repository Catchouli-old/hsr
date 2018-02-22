module Hasami.Renderer.GL
  ( createRenderer
  , toGLMat
  )
where

import Foreign
import Data.Vect
import SDL (($=))
import qualified SDL
import Control.Monad (unless)
import Control.Monad.IO.Class
import Graphics.GLUtil (readTexture, texture2DWrap)
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import Data.IORef
import qualified Data.Map.Strict as M

import Hasami.Renderer

-- Attribute locations
posAttrib :: GL.AttribLocation
uvsAttrib :: GL.AttribLocation
posAttrib = GL.AttribLocation 0
uvsAttrib = GL.AttribLocation 1

-- | Create a GL renderer
createRenderer :: SDL.Window -> IO Renderer
createRenderer win = do
  pure $ Renderer
    { swapBuffers = SDL.glSwapWindow win
    , renderClear = renderClear'
    , loadShader = loadShader'
    , loadTexture = loadTexture'
    , createBuffer = createBuffer'
    }

-- | Implementation of Renderer renderClear
renderClear' :: MonadIO m => Float -> Float -> Float -> Float -> m ()
renderClear' r g b a = liftIO $ do
  GL.clearColor $= GL.Color4 r g b a
  GL.clear [GL.ColorBuffer]

-- | Implementation of Renderer loadShader
loadShader' :: MonadIO m => FilePath -> m (Shader)
loadShader' path = liftIO $ do
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

  -- Bind our standard attrib locations
  GL.attribLocation prog "in_pos" $= posAttrib
  GL.attribLocation prog "in_uvs" $= uvsAttrib

  GL.linkProgram prog
  linkOK <- GL.get $ GL.linkStatus prog
  GL.validateProgram prog
  status <- GL.get $ GL.validateStatus prog
  unless (linkOK && status) $ do
    putStrLn "Error linking program"
    plog <- GL.get $ GL.programInfoLog prog
    putStrLn plog

  pure $ Shader { bindShader = GL.currentProgram $= Just prog
                , unbindShader = GL.currentProgram $= Nothing
                , setUniform = \name val -> do
                    GL.currentProgram $= Just prog
                    loc <- GL.get (GL.uniformLocation prog name)
                    setUniform' loc val
                }

-- | Implementation of Shader setUniform
setUniform' :: MonadIO m => GL.UniformLocation -> UniformValue -> m ()
setUniform' loc (UniformFloat f) = GL.uniform loc $= f
setUniform' loc (UniformTexture Texture0) = GL.uniform loc $= GL.TextureUnit 0
setUniform' loc (UniformMatrix mat) = liftIO $ (GL.uniform loc $=) =<< toGLMat mat

-- | Implementation of Renderer loadTexture
loadTexture' :: MonadIO m => FilePath -> m Texture
loadTexture' path = do
  t <- liftIO $ either error id <$> readTexture path
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  return $ Texture
    { bindTexture = \unit -> do
        GL.activeTexture $= unitToGl unit
        GL.textureBinding GL.Texture2D $= Just t
    }

-- | Texture unit to gl texture
unitToGl :: TextureUnit -> GL.TextureUnit
unitToGl _ = GL.TextureUnit 0

-- | Implementation of Renderer createBuffer
createBuffer' :: Storable a => V.Vector a -> Maybe Int32 -> Maybe Int32 -> Buffer a
createBuffer' bufferData posDims uvsDims = Buffer
  { drawBuffer = liftIO $ drawBuffer' bufferData posDims uvsDims
  }

-- | Implementation of Buffer drawBuffer
drawBuffer' :: Storable a => V.Vector a -> Maybe Int32 -> Maybe Int32 -> IO ()
drawBuffer' vec pos uvs = do
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
         GL.vertexAttribPointer posAttrib $=
           (GL.ToFloat, GL.VertexArrayDescriptor i GL.Float stride (plusPtr ptr posOffset))

  case uvs of
    Nothing -> GL.vertexAttribArray uvsAttrib $= GL.Disabled
    Just i -> do
      GL.vertexAttribArray uvsAttrib $= GL.Enabled
      V.unsafeWith vec $ \ptr ->
        GL.vertexAttribPointer uvsAttrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor i GL.Float stride (plusPtr ptr uvsOffset))

  GL.drawArrays GL.Triangles 0 elementCount

  GL.vertexAttribArray posAttrib $= GL.Disabled
  GL.vertexAttribArray uvsAttrib $= GL.Disabled

-- Renderer utils
class Mat a where
  toGLMat :: a -> IO (GL.GLmatrix Float)
  
instance Mat Mat4 where
  toGLMat (Mat4 (Vec4 a b c d) (Vec4 e f g h) (Vec4 i j k l) (Vec4 m n o p)) = do
    let mat = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]
    GL.newMatrix GL.ColumnMajor mat :: IO (GL.GLmatrix Float)

instance Mat Proj4 where
  toGLMat proj = toGLMat . fromProjective $ proj
