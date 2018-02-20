module Hasami.Window
  ( withGLWindow
  )
where

import qualified SDL

-- | Create an SDL window and run an io action using it
withGLWindow :: (SDL.Window -> IO ()) -> IO ()
withGLWindow act = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "" windowDef
  SDL.showWindow window

  _ <- SDL.glCreateContext window

  act window

  SDL.destroyWindow window
  SDL.quit

-- | Window definition
windowDef :: SDL.WindowConfig
windowDef = SDL.defaultWindow
  { SDL.windowInitialSize = SDL.V2 800 600
  , SDL.windowOpenGL = Just SDL.defaultOpenGL
  }
