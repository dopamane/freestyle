module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.Semigroup
import Freestyle
import Prettyprinter
import Prettyprinter.Render.Terminal

main :: IO ()
main = do
  f <- newFreestyleIO
  s <- newTVarIO $ stimes (4 :: Int) "FREESTYLE|"
  concurrently_ (runFreestyle f $ cfg s) $
    forever $ do
      atomically $ modifyTVar' s rote
      threadDelay 200000
  where
    rote s = last s : init s -- drop 1 s <> take 1 s

cfg :: TVar String -> FreestyleCfg String AnsiStyle
cfg s = FreestyleCfg
  { initState = readTVar s
  , drawState = \s' -> return $
    applyWhen (take 1 s' == "E") (annotate $ color Blue) $ pretty s'
  , layoutDoc = layoutPretty defaultLayoutOptions
  , renderDoc = renderLazy
  }
