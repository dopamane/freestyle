-- | Concurrent, pretty TUI
-- 
-- Example
-- @
-- main :: IO ()
-- main = do
--   f <- newFreestyleIO
--   s <- newTVarIO $ \"FREESTYLE!!!FREESTYLE!!!FREESTYLE!!!\"
--   concurrently_ (runFreestyle f $ cfg s) $
--     forever $ do
--       atomically $ modifyTVar' s rote
--       threadDelay 200000
--   where
--     rote s = last s : init s
--
-- cfg :: TVar String -> FreestyleCfg String AnsiStyle
-- cfg s = FreestyleCfg
--   { initState = readTVar s
--   , drawState = \\s' -> return $
--     applyWhen (take 1 s' == "E") (annotate $ color Blue) $ pretty s'
--   , layoutDoc = layoutPretty defaultLayoutOptions
--   , renderDoc = renderLazy
--   }
-- @
module Freestyle
  ( module Freestyle.Freestyle
  ) where

import Freestyle.Freestyle
