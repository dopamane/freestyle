{-# LANGUAGE OverloadedStrings #-}

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
-- The TUI updates when current state changes.
data FreestyleCfg s ann = FreestyleCfg
  { readState :: STM s                                -- ^ read the current state
  , drawState :: s -> STM (Doc ann)                   -- ^ draw the current state
  , layoutDoc :: Doc ann -> STM (SimpleDocStream ann) -- ^ layout the doc
  , renderDoc :: SimpleDocStream ann -> STM Text      -- ^ render the doc
  }

-- | Run the TUI with the configuration
runFreestyle :: Eq s => Freestyle s ann -> FreestyleCfg s ann -> IO a
runFreestyle f cfg = do
  atomically $ do
    setLayout f $ layoutDoc cfg
    setRender f $ renderDoc cfg
    setState  f $ readState cfg
    setDraw   f $ drawState cfg
  runState f

-- | Display the current state then wait a change to re-display
runState :: Eq s => Freestyle s ann -> IO a
runState f =
  -- without echo
  bracket_ (hSetEcho stdin False) (hSetEcho stdin True) $ do
    hSetBuffering stdout $ BlockBuffering Nothing
    -- without cursor
    bracket_ (output "\x1b[?25l") (output "\x1b[?25h") $
      loop Nothing `finally` output (eraseScreen <> home)
  where
    loop Nothing = join $ atomically $ do
      s <- join $ readTMVar (stateVar f)
      t <- renderText f s
      return $ do
        output $ eraseScreen <> home <> t
        loop $ Just (s, t)
    loop (Just (s, p)) = join $ atomically $ do
      s' <- join $ readTMVar (stateVar f)
      check $ s' /= s
      t <- renderText f s'
      return $ do
        output $ toLazyText $ composite t p
        loop $ Just (s', t)

renderText :: Freestyle s ann -> s -> STM Text
renderText f s = do
  draw <- readTMVar $ drawVar f
  layo <- readTMVar $ lay f
  rend <- readTMVar $ ren f
  rend =<< layo =<< draw s

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

home :: Text
home = "\x1b[H"

eraseLine :: Builder
eraseLine = "\x1b[0K"

eraseScreen :: Text
eraseScreen = "\x1b[2J"

movRow :: Int -> Builder
movRow n = "\x1b[" <> fromString (show (n + 1)) <> "H"

composite :: Text -> Text -> Builder
composite new old = go 0 (T.lines new) (T.lines old)
  where
    go _ ns [] = fromLazyText $ T.unlines ns
    go _ [] (_:_) = "\x1b[J"
    go r (n:ns) (o:os) = mconcat
      [ if n /= o then movRow r <> fromLazyText n else mempty
      , if T.length o > T.length n then eraseLine else mempty
      , go (r + 1) ns os
      ]
