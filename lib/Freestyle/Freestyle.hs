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

-- | Construct a new IO TUI
newFreestyleIO :: IO (Freestyle s ann)
newFreestyleIO = atomically newFreestyle

data FreestyleCfg s ann = FreestyleCfg
  { initState :: STM s
  , drawState :: s -> STM (Doc ann)
  , layoutDoc :: Doc ann -> SimpleDocStream ann
  , renderDoc :: SimpleDocStream ann -> Text
  }

runFreestyle :: Eq s => Freestyle s ann -> FreestyleCfg s ann -> IO a
runFreestyle f cfg = do
  initFreestyle f cfg
  either id id <$> race (runDisplay $ display f) (runState f)

-- | Initialize required configurations
initFreestyle :: Freestyle s ann -> FreestyleCfg s ann -> IO ()
initFreestyle f cfg = atomically $ do
  setLayout f $ layoutDoc cfg
  setRender f $ renderDoc cfg
  setState  f $ initState cfg
  setDraw   f $ drawState cfg

runState :: Eq s => Freestyle s ann -> IO a
runState f = forever $ join $ atomically $ do
  s <- join $ readTMVar (stateVar f)
  r <- readTMVar $ drawVar f
  displayDoc (display f) =<< r s
  return $ atomically $ do
    s' <- join $ readTMVar (stateVar f)
    check $ s /= s'

setLayout :: Freestyle s ann -> (Doc ann -> SimpleDocStream ann) -> STM ()
setLayout f = D.setLayout $ display f

setRender :: Freestyle s ann -> (SimpleDocStream ann -> Text) -> STM ()
setRender f = D.setRender $ display f

setState :: Freestyle s ann -> STM s -> STM ()
setState f = writeTMVar $ stateVar f

setDraw :: Freestyle s ann -> (s -> STM (Doc ann)) -> STM ()
setDraw f = writeTMVar $ drawVar f
