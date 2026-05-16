module Freestyle.Freestyle
  ( Freestyle
  , newFreestyle
  , runFreestyle
  , initFreestyle
  , FreestyleCfg(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Text.Lazy (Text)
import Freestyle.Display
import Prettyprinter

data Freestyle s ann = Freestyle
  { display  :: Display ann
  , stateVar :: TMVar (TVar s)             -- current state
  , drawVar  :: TMVar (s -> STM (Doc ann)) -- draw state
  }

newFreestyle :: STM (Freestyle s ann)
newFreestyle =
  Freestyle
    <$> newDisplay
    <*> newEmptyTMVar
    <*> newEmptyTMVar

runFreestyle :: Eq s => Freestyle s ann -> IO a
runFreestyle f =
  either id id <$> race (runDisplay $ display f) (runState f)

runState :: Eq s => Freestyle s ann -> IO a
runState f = forever $ join $ atomically $ do
  s <- readTVar =<< readTMVar (stateVar f)
  r <- readTMVar $ drawVar f
  displayDoc (display f) =<< r s
  return $ atomically $ do
    s' <- readTVar =<< readTMVar (stateVar f)
    check $ s /= s'

data FreestyleCfg s ann = FreestyleCfg
  { initState :: TVar s
  , drawState :: s -> STM (Doc ann)
  , layoutDoc :: Doc ann -> SimpleDocStream ann
  , renderDoc :: SimpleDocStream ann -> Text
  }

-- | Initialize required configurations
initFreestyle :: Freestyle s ann -> FreestyleCfg s ann -> STM ()
initFreestyle f cfg = do
  setLayout (display f) $ layoutDoc cfg
  setRender (display f) $ renderDoc cfg
  writeTMVar (stateVar f) $ initState cfg
  writeTMVar (drawVar  f) $ drawState cfg
