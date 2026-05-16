module Freestyle.Freestyle
  ( Freestyle
  , newFreestyle
  , initFreestyle
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
  , stateVar :: TMVar (STM s)
  , drawVar  :: TMVar (s -> IO (Doc ann))
  }

newFreestyle :: STM (Freestyle s ann)
newFreestyle =
  Freestyle
    <$> newDisplay
    <*> newEmptyTMVar
    <*> newEmptyTMVar

runFreestyle :: Eq s => Freestyle s ann -> IO a
runFreestyle f =
  either id id <$> race (runDisplay $ display f) (watchState f)

watchState :: Eq s => Freestyle s ann -> IO a
watchState f = forever $ join $ atomically $ do
  s <- join $ readTMVar $ stateVar f
  r <- readTMVar $ drawVar f
  return $ do
    atomically . displayDoc (display f) =<< r s
    atomically $ do
      s' <- join $ readTMVar $ stateVar f
      check $ s /= s'
  
data FreestyleCfg s ann = FreestyleCfg
  { readState :: STM s
  , drawState :: s -> IO (Doc ann)
  , layoutDoc :: Doc ann -> SimpleDocStream ann
  , renderDoc :: SimpleDocStream ann -> Text
  }

initFreestyle :: Freestyle s ann -> FreestyleCfg s ann -> STM ()
initFreestyle f cfg = do
  setLayout (display f) $ layoutDoc cfg
  setRender (display f) $ renderDoc cfg
  writeTMVar (stateVar f) $ readState cfg
  writeTMVar (drawVar  f) $ drawState cfg
