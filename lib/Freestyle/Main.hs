module Freestyle.Main (freestyleMain) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Freestyle.Freestyle
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import Prettyprinter.Render.Terminal
import System.IO

freestyleMain :: IO ()
freestyleMain = join $ atomically $ do
  f <- newFreestyle
  s <- newTVar ([Red, Green, Yellow, Blue], 0, False)
  initFreestyle f $ FreestyleCfg
    { initState = s
    , layoutDoc = layoutPretty defaultLayoutOptions
    , renderDoc = renderDisplay
    , drawState = \(cs, rads, en) -> return $
        vsep
          [ hcat $ zipWith renderColorChar (cycle cs) "~~~~~~~~~~ Freestyle!"
          , renderSin rads
          , pretty $ if en then "ON" else "OFF"
          ]
    }
  return $ do
    hSetBuffering stdin NoBuffering
    mapConcurrently_ id
      [ runFreestyle f
      , forever $ do
          atomically $ modifyTVar' s $ \(cs, offset, en) ->
            (drop 1 cs <> take 1 cs, offset + 0.1, en)
          threadDelay 60000
      , forever $ do
        ch <- getChar
        when (ch == 'a') $
          atomically $ modifyTVar' s $ \(cs, offset, en) -> (cs, offset, not en)
      ]

renderSin :: Double -> Doc ann
renderSin offset = vsep [renderRow r | r <- [-5..5]]
  where
    renderRow r = hcat [renderCell c | c <- [0..31]]
      where
        renderCell c
          | floor (y * 5) == (r :: Integer) = pretty "*"
          | otherwise = pretty " "
          where
            y = sin (c / 10 + offset)

renderColorChar :: Color -> Char -> Doc Style
renderColorChar clr ch = annotate (Ansi $ color clr) $ pretty ch

renderDisplay :: SimpleDocStream Style -> Text
renderDisplay = renderSimplyDecorated T.fromStrict render . treeForm

render :: Style -> Text -> Text
render d s = case d of
  Ansi a -> go $ annotate a $ pretty s
  Title  -> go $ annotate (bold <> underlined <> color Blue) $ pretty s
  where
    go = renderLazy . layoutPretty defaultLayoutOptions

data Style
  = Ansi AnsiStyle
  | Title
  deriving (Eq, Show)
