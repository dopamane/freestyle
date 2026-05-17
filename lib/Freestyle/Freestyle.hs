-- | Freestyle TUI
module Freestyle.Freestyle
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

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Text.Lazy (Text)
import Freestyle.Display
import Prettyprinter

-- | TUI handle
data Freestyle s ann = Freestyle
  { display  :: Display ann
  , stateVar :: TMVar (STM s)              -- current state
  , drawVar  :: TMVar (s -> STM (Doc ann)) -- draw state
  }

-- | Construct a new STM TUI
newFreestyle :: STM (Freestyle s ann)
newFreestyle =
  Freestyle
    <$> newDisplay
    <*> newEmptyTMVar
    <*> newEmptyTMVar

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
  atomically $ initFreestyle f cfg
  either id id <$> race (runDisplay $ display f) (runState f)

-- | Initialize required configurations
initFreestyle :: Freestyle s ann -> FreestyleCfg s ann -> STM ()
initFreestyle f cfg = do
  setLayout f $ layoutDoc cfg
  setRender f $ renderDoc cfg
  setState  f $ readState cfg
  setDraw   f $ drawState cfg

-- | Display the current state then wait a change to re-display
runState :: Eq s => Freestyle s ann -> IO a
runState f = forever $ join $ atomically $ do
  s <- join $ readTMVar (stateVar f)
  r <- readTMVar $ drawVar f
  displayDoc (display f) =<< r s
  return $ atomically $ do
    s' <- join $ readTMVar (stateVar f)
    check $ s /= s'

-- | Change the layout algorithm
setLayout :: Freestyle s ann -> (Doc ann -> STM (SimpleDocStream ann)) -> STM ()
setLayout f = setDisplayLayout $ display f

-- | Change the rendering algorithm
setRender :: Freestyle s ann -> (SimpleDocStream ann -> STM Text) -> STM ()
setRender f = setDisplayRender $ display f

-- | Change the state reader
setState :: Freestyle s ann -> STM s -> STM ()
setState f = writeTMVar $ stateVar f

-- | Change the drawing algorithm
setDraw :: Freestyle s ann -> (s -> STM (Doc ann)) -> STM ()
setDraw f = writeTMVar $ drawVar f
