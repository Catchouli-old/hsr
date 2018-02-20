module App
  ( initApp
  , runApp
  , app
  )
where

import SDL (($=))
import qualified SDL
import Control.Monad (unless)
import Control.Monad.State
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as V
import Data.Vect
import Foreign

import Hasami.Renderer
import Hasami.Resources

-- | App state monad
newtype App a = App {unpackApp :: StateT AppState RS a} deriving (Monad, Applicative, Functor, MonadIO, MonadState AppState)

-- | Run app state
runApp :: App a -> AppState -> RS (a, AppState)
runApp a = runStateT (unpackApp a)

-- | Lift RS into app
liftRS :: RS a -> App a
liftRS = App . lift

-- | Run IO in an RS in our app monad
liftRSIO :: RS (IO a) -> App a
liftRSIO = liftRS >=> liftIO

-- | App state
data AppState = AppState
  { _shaderProgram :: GL.Program
  , _texture :: GL.TextureObject
  }
makeLenses ''AppState

-- | Initialise app
initApp :: RS (AppState)
initApp = do
  prog <- liftIO $ resourcePath >=> loadShader $ "sprite.glsl"
  tex <- liftIO $ resourcePath >=> loadTex $ "patchouli.png"
  pure $ AppState
    { _shaderProgram = prog
    , _texture = tex
    }

-- | Application
app :: App ()
app = loop

-- | Main loop
loop :: App ()
loop = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

  prog <- use shaderProgram
  tex <- use texture

  liftIO $ do
    time <- SDL.time

    let offset = Vec3 (sin time) 0 0
    let trans = translate4 offset idmtx

    GL.clearColor $= GL.Color4 1 0 1 1
    GL.clear [GL.ColorBuffer]

    mvpLoc <- GL.get (GL.uniformLocation prog "uni_mvp")
    texLoc <- GL.get (GL.uniformLocation prog "uni_tex")

    let vertices = V.fromList @Float [ -0.5, -0.5, 0.0, 1.0
                                     ,  0.5, -0.5, 1.0, 1.0
                                     ,  0.5,  0.5, 1.0, 0.0
                                     ,  0.5,  0.5, 1.0, 0.0
                                     , -0.5,  0.5, 0.0, 0.0
                                     , -0.5, -0.5, 0.0, 1.0
                                     ]

    GL.currentProgram $= Just prog

    GL.activeTexture $= GL.TextureUnit 0
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just tex

    GL.uniform texLoc $= GL.TextureUnit 0
    (GL.uniform mvpLoc $=) =<< toGLMat trans

    GL.vertexAttribArray posAttrib $= GL.Enabled
    GL.vertexAttribArray uvsAttrib $= GL.Enabled
    V.unsafeWith vertices $ \ptr ->
      GL.vertexAttribPointer posAttrib $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 16 ptr)
    V.unsafeWith vertices $ \ptr ->
      GL.vertexAttribPointer uvsAttrib $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 16 (plusPtr ptr 8))
    GL.drawArrays GL.Triangles 0 6
    GL.vertexAttribArray posAttrib $= GL.Disabled
    GL.vertexAttribArray uvsAttrib $= GL.Disabled

  liftRSIO $ use swapBuffers

  unless quit loop

