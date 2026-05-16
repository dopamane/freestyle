module Freestyle.Main (freestyleMain) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Semigroup
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Freestyle.Freestyle
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import Prettyprinter.Render.Terminal

freestyleMain :: IO ()
freestyleMain = join $ atomically $ do
  f <- newFreestyle
  s <- newTVar ( [Red, Green, Yellow, Blue]
               , 0
               , False
               , [ rotate i r
                 | (i, r) <- zip [1..] $ replicate 8 $ stimes 4 [Blue, Green, Blue, Blue, Yellow, Red, Green, Blue]]
               )
  initFreestyle f $ FreestyleCfg
    { initState = s
    , layoutDoc = layoutPretty defaultLayoutOptions
    , renderDoc = renderDisplay
    , drawState = \(cs, rads, en, wf) -> return $
        vsep
          [ hcat $ zipWith renderColorChar (cycle cs) "~~~~~~~~~~ Freestyle!"
          , renderSin rads
          , pretty $ if en then "ON" else "OFF"
          , renderWaterfall wf
          ]
    }
  return $ mapConcurrently_ id
    [ runFreestyle f
    , forever $ do
        atomically $ modifyTVar' s $ \(cs, offset, en, wf) ->
          (drop 1 cs <> take 1 cs, offset + 0.1, en, wf)
        threadDelay 60000
    , forever $ do
      ch <- getChar
      when (ch == 'a') $
        atomically $ modifyTVar' s $ \(cs, offset, en, wf) ->
          (cs, offset, not en, wf)
    , forever $ do
        atomically $ modifyTVar' s $ \(cs, offset, en, wf) ->
          (cs, offset, en, cycleWaterfall wf)
        threadDelay 60000
    ]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

renderWaterfall :: [[Color]] -> Doc Style
renderWaterfall rs = vsep [renderRow r | r <- rs]
  where
    renderRow r = hcat [renderCell c | c <- r]
      where
        renderCell c = annotate (Ansi $ bgColor c) space

cycleWaterfall :: [[Color]] -> [[Color]]
cycleWaterfall w = drop 1 w <> take 1 w

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
