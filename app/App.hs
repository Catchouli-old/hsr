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
import Hasami.Renderer.GL
import Hasami.Resources

-- | App state monad
newtype App a = App {unpackApp :: StateT AppState IO a} deriving (Monad, Applicative, Functor, MonadIO, MonadState AppState)

-- | Run app state
runApp :: App a -> AppState -> IO (a, AppState)
runApp a = runStateT (unpackApp a)

-- | App state
data AppState = AppState
  { _shaderProgram :: Shader
  , _texture :: Texture
  , _nuklearContext :: NK
  , _renderer' :: Renderer
  }
makeLenses ''AppState

-- | Initialise app
initApp :: SDL.Window -> IO (AppState)
initApp win = do
  renderer <- liftIO $ createRenderer win
  prog <- resourcePath >=> loadShader renderer $ "sprite.glsl"
  tex <- resourcePath >=> loadTexture renderer $ "patchouli.png"
  nk <- initNuklear win
  _ <- nuklearInitAtlas
  pure $ AppState
    { _shaderProgram = prog
    , _texture = tex
    , _nuklearContext = nk
    , _renderer' = renderer
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

  renderer <- use renderer'
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
    let buf = createBuffer renderer vertices (Just 2) (Just 2)

    -- Clear window
    renderClear renderer 1 0 1 1

    bindShader shader
    bindTexture tex Texture0
    setUniform shader "uni_tex" $ UniformTexture Texture0
    setUniform shader "uni_mvp" $ UniformMatrix . fromProjective $ trans
    drawBuffer buf
    unbindShader shader

    test nk
    nuklearRender

  liftIO $ swapBuffers renderer

  unless quit loop

