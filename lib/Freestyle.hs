-- | Concurrent, pretty TUI
--
-- @
-- import Freestyle
--
-- main :: IO ()
-- main = do
--   s <- newTVarIO \"FREESTYLE!!!FREESTYLE!!!FREESTYLE!!!\"
--   f <- newFreestyleIO
--   concurrently_ (runFreestyle f $ cfg s) $
--     forever $ do
--       atomically $ modifyTVar' s rote
--       threadDelay 200000
--   where
--     rote s = last s : init s
--
-- cfg :: TVar String -> FreestyleCfg String AnsiStyle
-- cfg s = FreestyleCfg
--   { readState = readTVar s
--   , drawState = \\s' -> return $
--     applyWhen (take 1 s' == \"E\") (annotate $ color Blue) $ pretty s'
--   , layoutDoc = return . layoutPretty defaultLayoutOptions
--   , renderDoc = return . renderLazy
--   }
-- @
module Freestyle
  ( Freestyle
  , newFreestyle
  , newFreestyleIO
  , FreestyleCfg(..)
  , runFreestyle
  , setLayout
  , setRender
  , setState
  , setDraw
  ) where

import Freestyle.Freestyle
