module Freestyle.Freestyle
  ( Freestyle
  , newFreestyle
  , FreestyleCfg(..)
  , runFreestyle
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Text.Lazy (Text)
import Freestyle.Display
import Prettyprinter

data Freestyle s ann = Freestyle
  { display  :: Display ann
  , stateVar :: TMVar (STM s)              -- current state
  , drawVar  :: TMVar (s -> STM (Doc ann)) -- draw state
  }

newFreestyle :: STM (Freestyle s ann)
newFreestyle =
  Freestyle
    <$> newDisplay
    <*> newEmptyTMVar
    <*> newEmptyTMVar

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
  setLayout (display f) $ layoutDoc cfg
  setRender (display f) $ renderDoc cfg
  writeTMVar (stateVar f) $ initState cfg
  writeTMVar (drawVar  f) $ drawState cfg

runState :: Eq s => Freestyle s ann -> IO a
runState f = forever $ join $ atomically $ do
  s <- join $ readTMVar (stateVar f)
  r <- readTMVar $ drawVar f
  displayDoc (display f) =<< r s
  return $ atomically $ do
    s' <- join $ readTMVar (stateVar f)
    check $ s /= s'
