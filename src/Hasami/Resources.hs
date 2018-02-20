module Hasami.Resources
  ( resourcePath
  )
where

-- Cabal data-files is disabled for development since otherwise you can't refresh the files in ghci

--import Paths_hasami

-- | Resource path
resourcePath :: String -> IO String
resourcePath = pure . id . ("res/"++)
--resourcePath = getDataFileName . ("res/"++)
