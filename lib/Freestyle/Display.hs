module Freestyle.Display
  ( Display
  , newDisplay
  , runDisplay
  , setLayout
  , setRender
  , displayDoc
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Prettyprinter
import System.IO

data Display ann = Display
  { str :: TMVar (SimpleDocStream ann)
  , lay :: TMVar (Doc ann -> SimpleDocStream ann)
  , ren :: TMVar (SimpleDocStream ann -> Text)
  }

newDisplay :: STM (Display ann)
newDisplay =
  Display <$> newEmptyTMVar <*> newEmptyTMVar <*> newEmptyTMVar

setLayout :: Display ann -> (Doc ann -> SimpleDocStream ann) -> STM ()
setLayout e = writeTMVar $ lay e

setRender :: Display ann -> (SimpleDocStream ann -> Text) -> STM ()
setRender e = writeTMVar $ ren e

displayDoc :: Display ann -> Doc ann -> STM ()
displayDoc e d = do
  l <- readTMVar (lay e) `orElse` throwSTM NoLayoutError
  _ <- readTMVar (ren e) `orElse` throwSTM NoRenderError
  writeTMVar (str e) $ l d

runDisplay :: Display ann -> IO a
runDisplay e = withoutEcho $ do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout LineBuffering
  withoutCursor $
    forever $ join $ atomically $ do
      s <- takeTMVar $ str e
      r <- readTMVar $ ren e
      -- clear the screen, set cursor back to top left
      -- then output text
      return $ TIO.putStrLn $ T.pack "\x1b[2J\x1b[H" <> r s

withoutCursor :: IO a -> IO a
withoutCursor =
  bracket_
    (TIO.putStrLn $ T.pack "\x1b[?25l") -- hide cursor
    (TIO.putStrLn $ T.pack "\x1b[?25h") -- show cursor

withoutEcho :: IO a -> IO a
withoutEcho =
  bracket_
    (hSetEcho stdin False)
    (hSetEcho stdin True)

data DisplayException
  = NoLayoutError
  | NoRenderError
  deriving (Eq, Read)

instance Show DisplayException where
  show NoLayoutError = "no layout, use setLayout"
  show NoRenderError = "no render, use setRender"

instance Exception DisplayException
