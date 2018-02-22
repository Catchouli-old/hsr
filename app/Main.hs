module Main where

import Hasami.Window

import App

-- | Main
main :: IO ()
main = withGLWindow $ \window -> do
  _ <- initApp window >>= runApp app
  pure ()
