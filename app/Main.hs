module Main where

import Hasami.Window
import Hasami.Renderer

import App

-- | Main
main :: IO ()
main = withGLWindow $ \window -> do
  initialState <- initRenderer window
  _ <- runRS (initApp >>= runApp app) initialState
  pure ()
