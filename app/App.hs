module App
  ( initApp
  , runApp
  , app
  )
where

import qualified SDL
import Control.Monad (unless)
import Control.Monad.State hiding (withState)
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
  , _frameTimeCPU :: Float
  , _frameTimeTotal :: Float
  }
makeLenses ''AppState

-- | Initialise app
initApp :: SDL.Window -> IO (AppState)
initApp win = do
  renderer <- liftIO $ createRenderer win
  prog <- resourcePath >=> loadShader renderer $ "sprite.glsl"
  tex <- resourcePath >=> loadTexture renderer $ "patchouli.png"
  nk <- initNuklear win
  pure $ AppState
    { _shaderProgram = prog
    , _texture = tex
    , _nuklearContext = nk
    , _renderer' = renderer
    , _frameTimeCPU = 0
    , _frameTimeTotal = 0
    }

-- | Application
app :: App ()
app = do
  loop
  nk <- use nuklearContext
  liftIO $ nuklearShutdown nk

-- | Main loop
loop :: App ()
loop = do
  time <- liftIO $ SDL.time

  evt <- pollEvents'
  let rawEvents = map snd evt
  let events = map fst evt
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

  renderer <- use renderer'
  shader <- use shaderProgram
  tex <- use texture
  nk <- use nuklearContext

  ftCpu <- use frameTimeCPU
  ftTotal <- use frameTimeTotal

  liftIO $ nuklearHandleEvents nk rawEvents

  liftIO $ do
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
    withState renderer [ClearColor 0 0 1 1] $ do
      renderClear renderer

      bindShader shader
      bindTexture tex Texture0
      setUniform shader "uni_tex" $ UniformTexture Texture0
      setUniform shader "uni_mvp" $ UniformMatrix . fromProjective $ trans
      drawBuffer buf
      unbindShader shader

    nkWindow nk "Debug" defaultWindow $ do
      nkLayoutDynamic nk 20 1
      nkLabel nk $ "Frame time (cpu): " ++ show ftCpu
      nkLabel nk $ "Frame time (total): " ++ show ftTotal
    nuklearRender nk

  beforeSwapBuffers <- liftIO $ SDL.time
  frameTimeCPU .= (beforeSwapBuffers - time) * 1000

  liftIO $ swapBuffers renderer

  afterSwapBuffers <- liftIO $ SDL.time
  frameTimeTotal .= (afterSwapBuffers - time) * 1000

  unless quit loop

