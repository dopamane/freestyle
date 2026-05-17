module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Freestyle
import Prettyprinter
import Prettyprinter.Render.Terminal

main :: IO ()
main = do
  s <- newTVarIO "FREESTYLE!!!FREESTYLE!!!FREESTYLE!!!"
  f <- newFreestyleIO
  concurrently_ (runFreestyle f $ cfg s) $
    forever $ do
      atomically $ modifyTVar' s rote
      threadDelay 200000
  where
    rote s = last s : init s

cfg :: TVar String -> FreestyleCfg String AnsiStyle
cfg s = FreestyleCfg
  { readState = readTVar s
  , drawState = \s' -> return $
    applyWhen (take 1 s' == "E") (annotate $ color Blue) $ pretty s'
  , layoutDoc = layoutPretty defaultLayoutOptions
  , renderDoc = renderLazy
  }
