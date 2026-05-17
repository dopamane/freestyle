-- | Display text on terminal
module Freestyle.Display
  ( Display
  , newDisplay
  , runDisplay
  , setDisplayLayout
  , setDisplayRender
  , displayDoc
  , DisplayException(..)
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Prettyprinter
import System.IO

-- | Display handle
data Display ann = Display
  { str :: TMVar (SimpleDocStream ann)
  , lay :: TMVar (Doc ann -> STM (SimpleDocStream ann))
  , ren :: TMVar (SimpleDocStream ann -> STM Text)
  }

-- | Construct a new display handle
newDisplay :: STM (Display ann)
newDisplay =
  Display <$> newEmptyTMVar <*> newEmptyTMVar <*> newEmptyTMVar

-- | Set the layout algorithm
setDisplayLayout :: Display ann -> (Doc ann -> STM (SimpleDocStream ann)) -> STM ()
setDisplayLayout e = writeTMVar $ lay e

-- | Set the rendering algorithm
setDisplayRender :: Display ann -> (SimpleDocStream ann -> STM Text) -> STM ()
setDisplayRender e = writeTMVar $ ren e

-- | Layout, render, then send to the display daemon.
-- If a layout or rendering algorithm is not present
-- throws 'DisplayException'.
displayDoc :: Display ann -> Doc ann -> STM ()
displayDoc e d = do
  l <- readTMVar (lay e) `orElse` throwSTM NoLayoutError
  _ <- readTMVar (ren e) `orElse` throwSTM NoRenderError
  writeTMVar (str e) =<< l d

-- | Setup the terminal buffering, echo, cursor.
-- Then repeatedly read the doc stream, render text
-- and output to terminal.
runDisplay :: Display ann -> IO a
runDisplay e =
  withTerm $
    forever $ join $ atomically $ do
      s <- takeTMVar $ str e
      r <- readTMVar $ ren e
      t <- r s
      -- clear the screen, set cursor back to top left
      -- then output text
      return $ TIO.putStrLn $ T.pack "\x1b[2J\x1b[H" <> t

withTerm :: IO a -> IO a
withTerm k = withoutEcho $ do
  hSetBuffering stdout LineBuffering
  withoutCursor k

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

-- | Freestyle display exception
data DisplayException
  = NoLayoutError -- ^ no layout, use 'setDisplayLayout'
  | NoRenderError -- ^ no render, use 'setDisplayRender'
  deriving (Eq, Read)

instance Show DisplayException where
  show NoLayoutError = "no layout, use setDisplayLayout"
  show NoRenderError = "no render, use setDisplayRender"

instance Exception DisplayException
