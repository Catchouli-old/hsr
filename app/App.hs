module App
  ( initApp
  , runApp
  , app
  )
where

import qualified SDL
import Control.Monad (unless)
import Control.Monad.State
import Control.Lens
import qualified Data.Vector.Storable as V
import Data.Vect

import Nuklear
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
--liftRSIO :: RS (IO a) -> App a
--liftRSIO = liftRS >=> liftIO

-- | App state
data AppState = AppState
  { _shaderProgram :: Shader
  , _texture :: Texture
  , _nuklearContext :: NK
  }
makeLenses ''AppState

-- | Initialise app
initApp :: RS (AppState)
initApp = do
  prog <- liftIO $ resourcePath >=> loadShader $ "sprite.glsl"
  tex <- liftIO $ resourcePath >=> loadTex $ "patchouli.png"
  win <- use rendererWindow
  nk <- liftIO $ initNuklear win
  _ <- liftIO $ nuklearInitAtlas
  pure $ AppState
    { _shaderProgram = prog
    , _texture = tex
    , _nuklearContext = nk
    }

-- | Application
app :: App ()
app = loop

-- | Main loop
loop :: App ()
loop = do
  evt <- pollEvents'
  let rawEvents = map snd evt
  let events = map fst evt
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

  shader <- use shaderProgram
  tex <- use texture
  nk <- use nuklearContext

  liftIO $ nuklearHandleEvents nk rawEvents

  liftIO $ do
    -- Get current time
    time <- SDL.time

    -- Set up projection matrix
    let offset = Vec3 (sin time) 0 0
    let trans = translate4 offset idmtx

    -- Vertex buffer
    let vertices = V.fromList @Float [ -0.5, -0.5, 0.0, 1.0
                                     ,  0.5, -0.5, 1.0, 1.0
                                     ,  0.5,  0.5, 1.0, 0.0
                                     ,  0.5,  0.5, 1.0, 0.0
                                     , -0.5,  0.5, 0.0, 0.0
                                     , -0.5, -0.5, 0.0, 1.0
                                     ]
    let buf = Buffer vertices (Just 2) (Just 2)

    -- Clear window
    renderClear 1 0 1 1

    bindShader shader
    bindTexture shader "uni_tex" 0 tex
    setUniform shader "uni_mvp" =<< toGLMat trans
    drawBuffer buf
    unbindShader

    test nk
    nuklearRender

  liftRS $ swapBuffers

  unless quit loop

