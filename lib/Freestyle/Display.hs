{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text.Lazy.Builder
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

-- | write stdout and flush
output :: Text -> IO ()
output t = TIO.putStr t >> hFlush stdout

-- | Setup the terminal buffering, echo, cursor.
-- Then repeatedly read the doc stream, render text
-- and output to terminal.
runDisplay :: Display ann -> IO a
runDisplay e =
  withTerm $ loop Nothing `finally` output (eraseScreen <> home)
  where
    loop prevM = join $ atomically $ do
      s <- takeTMVar $ str e
      r <- readTMVar $ ren e
      t <- r s
      return $ do
        output $ case prevM of
          Nothing -> eraseScreen <> home <> t
          Just  p -> toLazyText $ composite t p
        loop $ Just t

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
    go _ [] [] = mempty
    go _ ns [] = fromLazyText $ T.unlines ns
    go _ [] (_:_) = "\x1b[J"
    go r (n:ns) (o:os)
      | n /= o = movRow r <> fromLazyText n <> eraseLine <> go (r + 1) ns os
      | otherwise = go (r + 1) ns os

withTerm :: IO a -> IO a
withTerm k = withoutEcho $ do
  hSetBuffering stdout $ BlockBuffering Nothing
  withoutCursor k

withoutCursor :: IO a -> IO a
withoutCursor =
  bracket_
    (output "\x1b[?25l") -- hide cursor
    (output "\x1b[?25h") -- show cursor

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
