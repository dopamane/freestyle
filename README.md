# Freestyle

Concurrent, pretty TUI

Example
```hs
import Freestyle

main :: IO ()
main = do
  s <- newTVarIO "FREESTYLE!!!FREESTYLE!!!FREESTYLE!!!"
  concurrently_ (runFreestyle $ cfg s) $
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
```

Development
```
cabal build
cabal run
cabal haddock
```
