# Freestyle

Create and display pretty terminal graphics

Construct a new handle and configuration:

  * a way to read state
  * a function to draw the current state
  * a function to layout the current doc
  * a function to render the `SimpleDocStream` to `Text`

Run the handle and configuration using `runFreestyle`.
Note, it has an `Eq ann` constraint. Freestyle re-renders
the terminal if the `SimpleDocStream ann` changes.
This way, apps may make state changes without causing
terminal re-rendering.

The `Freestyle s ann` handle supports on-the-fly
state, draw, layout, and rendering changes. Use dynamic
layout algorithms based on window size or environment.

Example
```hs
import Freestyle

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
  , layoutDoc = return . layoutPretty defaultLayoutOptions
  , renderDoc = return . renderLazy
  }
```

Development
```
cabal build
cabal run
cabal haddock
```
