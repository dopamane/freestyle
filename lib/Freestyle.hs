{-# LANGUAGE OverloadedStrings #-}

-- | Create and display pretty terminal graphics
--
-- Construct a new handle and configuration:
--
--   * a way to read state
--   * a function to draw the current state
--   * a function to layout the current doc
--   * a function to render the 'SimpleDocStream' to 'Text'
--
-- Run the configuration using 'runFreestyle'.
-- Note, it has an 'Eq' constraint. Freestyle re-renders
-- the terminal if the 'SimpleDocStream' changes.
-- This way, apps may make state changes without causing
-- terminal re-rendering.
--
-- The t'Freestyle' handle supports on-the-fly
-- state, draw, layout, and rendering changes. Use dynamic
-- layout, rendering algorithms based on environment
-- such as window size.
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

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as TIO
import Prettyprinter
import System.IO

-- | TUI handle
data Freestyle s ann = Freestyle
  { lay      :: TMVar (Doc ann -> STM (SimpleDocStream ann))
  , ren      :: TMVar (SimpleDocStream ann -> STM Text)
  , stateVar :: TMVar (STM s)              -- current state
  , drawVar  :: TMVar (s -> STM (Doc ann)) -- draw state
  }

-- | Construct a new STM TUI
newFreestyle :: STM (Freestyle s ann)
newFreestyle =
  Freestyle <$> newEmptyTMVar <*> newEmptyTMVar <*> newEmptyTMVar <*> newEmptyTMVar

-- | Construct a new IO TUI
newFreestyleIO :: IO (Freestyle s ann)
newFreestyleIO = atomically newFreestyle

-- | User init configuration.
-- The TUI updates when 'SimpleDocStream' changes.
data FreestyleCfg s ann = FreestyleCfg
  { readState :: STM s                                -- ^ read the current state
  , drawState :: s -> STM (Doc ann)                   -- ^ draw the current state
  , layoutDoc :: Doc ann -> STM (SimpleDocStream ann) -- ^ layout the doc
  , renderDoc :: SimpleDocStream ann -> STM Text      -- ^ render the doc
  }

-- | Run the TUI with the configuration
runFreestyle :: Eq ann => Freestyle s ann -> FreestyleCfg s ann -> IO a
runFreestyle f cfg = do
  atomically $ do
    setLayout f $ layoutDoc cfg
    setRender f $ renderDoc cfg
    setState  f $ readState cfg
    setDraw   f $ drawState cfg
  -- without echo
  bracket_ (hSetEcho stdin False) (hSetEcho stdin True) $ do
    hSetBuffering stdout $ BlockBuffering Nothing
    -- without cursor
    bracket_ (output "\x1b[?25l") (output "\x1b[?25h") $
      loop Nothing `finally` output clearScreen
  where
    loop prevM = join $ atomically $ do
      s <- join $ readTMVar $ stateVar f
      draw <- readTMVar $ drawVar f
      layo <- readTMVar $ lay f
      rend <- readTMVar $ ren f
      sds' <- layo =<< draw s
      case prevM of
        Nothing -> do
          t <- rend sds'
          return $ do
            output $ clearScreen <> t
            loop $ Just (sds', t)
        Just (sds, p) -> do
          check $ sds /= sds'
          t <- rend sds'
          return $ do
            output $ composite t p
            loop $ Just (sds', t)

-- | Change the layout algorithm
setLayout :: Freestyle s ann -> (Doc ann -> STM (SimpleDocStream ann)) -> STM ()
setLayout f = writeTMVar $ lay f

-- | Change the rendering algorithm
setRender :: Freestyle s ann -> (SimpleDocStream ann -> STM Text) -> STM ()
setRender f = writeTMVar $ ren f

-- | Change the state reader
setState :: Freestyle s ann -> STM s -> STM ()
setState f = writeTMVar $ stateVar f

-- | Change the drawing algorithm
setDraw :: Freestyle s ann -> (s -> STM (Doc ann)) -> STM ()
setDraw f = writeTMVar $ drawVar f

-- | write stdout and flush
output :: Text -> IO ()
output t = TIO.putStr t >> hFlush stdout

-- | erase screen and goto home
clearScreen :: Text
clearScreen = "\x1b[2J\x1b[H"

-- | construct line diff rewrites
composite :: Text -> Text -> Text
composite new old = toLazyText $ go 0 (T.lines new) (T.lines old)
  where
    go _ ns [] = fromLazyText $ T.unlines ns
    go _ [] (_:_) = "\x1b[J"
    go r (n:ns) (o:os) = mconcat
      [ if n /= o then movRow <> fromLazyText n else mempty
      , if T.length o > T.length n then "\x1b[0K" else mempty -- erase line
      , go (r + 1) ns os
      ]
      where
        movRow = "\x1b[" <> fromString (show (r + 1 :: Int)) <> "H"
