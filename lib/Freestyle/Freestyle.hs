module Freestyle.Freestyle
  ( FreestyleCfg(..)
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
import Freestyle.Display hiding (setLayout, setRender)
import qualified Freestyle.Display as D (setLayout, setRender)
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

-- | User init configuration
data FreestyleCfg s ann = FreestyleCfg
  { initState :: STM s                          -- ^ read the current state
  , drawState :: s -> STM (Doc ann)             -- ^ draw the current state
  , layoutDoc :: Doc ann -> SimpleDocStream ann -- ^ layout the doc
  , renderDoc :: SimpleDocStream ann -> Text    -- ^ render the doc
  }

-- | Run the TUI with the configuration
runFreestyle :: Eq s => FreestyleCfg s ann -> IO a
runFreestyle cfg = join $ atomically $ do
  f <- newFreestyle
  initFreestyle f cfg
  return $ either id id <$> race (runDisplay $ display f) (runState f)

-- | Initialize required configurations
initFreestyle :: Freestyle s ann -> FreestyleCfg s ann -> STM ()
initFreestyle f cfg = do
  setLayout f $ layoutDoc cfg
  setRender f $ renderDoc cfg
  setState  f $ initState cfg
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
setLayout :: Freestyle s ann -> (Doc ann -> SimpleDocStream ann) -> STM ()
setLayout f = D.setLayout $ display f

-- | Change the rendering algorithm
setRender :: Freestyle s ann -> (SimpleDocStream ann -> Text) -> STM ()
setRender f = D.setRender $ display f

-- | Change the state accesor
setState :: Freestyle s ann -> STM s -> STM ()
setState f = writeTMVar $ stateVar f

-- | Change the drawing algorithm
setDraw :: Freestyle s ann -> (s -> STM (Doc ann)) -> STM ()
setDraw f = writeTMVar $ drawVar f
