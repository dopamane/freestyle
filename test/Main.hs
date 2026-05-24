module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Semigroup
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Freestyle
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import Prettyprinter.Render.Terminal
import System.IO

main :: IO ()
main = join $ atomically $ do
  s <- newTVar initMain
  f <- newFreestyle
  return $ mapConcurrently_ id
    [ runFreestyle f $ mainCfg s
    , runWheel s
    , runKeyReader s
    , runWaterfall s
    ]

data Main = Main
  { colorWheel :: [Color]
  , offset :: Double
  , switch :: Bool
  , waterfall :: [[Color]]
  }
  deriving Eq

initMain :: Main
initMain = Main
  { colorWheel = [Red, Green, Yellow, Blue]
  , offset = 0
  , switch = False
  , waterfall =
    [ rotate i r
    | (i, r) <- zip [1..] $ replicate 25 $ stimes (5 :: Int)
      [Blue, Green, Blue, Blue, Yellow, Red, Cyan, Green, Blue]
    ]
  }

mainCfg :: TVar Main -> FreestyleCfg Main Style
mainCfg s = FreestyleCfg
  { readState = readTVar s
  , layoutDoc = return . layoutPretty defaultLayoutOptions
  , renderDoc = return . renderDisplay
  , drawState = mainDraw
  }

mainDraw :: Main -> STM (Doc Style)
mainDraw (Main cs rads en wf) =
  return $ vsep
    [ hcat $ zipWith renderColorChar (cycle cs) "~~~~~~~~~~ Freestyle!"
    , renderSin rads
    , pretty $ if en then "ON" else "OFF"
    , drawWaterfall wf
    ]

runWheel :: TVar Main -> IO a
runWheel s = forever $ do
  atomically $ modifyTVar' s updateWheel
  threadDelay 100000
  where
    updateWheel m@(Main cs o _ _) = m{colorWheel=cs', offset=o'}
      where
        cs' = drop 1 cs <> take 1 cs
        o'  = o + 0.1

runKeyReader :: TVar Main -> IO a
runKeyReader s = do
  hSetBuffering stdin NoBuffering
  forever $ do
    ch <- getChar
    when (ch == 'a') $
      atomically $ modifyTVar' s $ \m -> m{switch=not $ switch m}

runWaterfall :: TVar Main -> IO a
runWaterfall s = forever $ do
  atomically $ modifyTVar' s $ \m ->
    m{waterfall=cycleWaterfall $ waterfall m}
  threadDelay 50000

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

drawWaterfall :: [[Color]] -> Doc Style
drawWaterfall rs = vsep [renderRow r | r <- rs]
  where
    renderRow r = hcat [renderCell c | c <- r]
      where
        renderCell c = annotate (Ansi $ bgColor c) space

cycleWaterfall :: [[Color]] -> [[Color]]
cycleWaterfall w = drop 1 w <> take 1 w

renderSin :: Double -> Doc ann
renderSin o = vsep [renderRow r | r <- [-5..5]]
  where
    renderRow r = hcat [renderCell c | c <- [0..31]]
      where
        renderCell c
          | floor (y * 5) == (r :: Integer) = pretty "*"
          | otherwise = pretty " "
          where
            y = sin (c / 10 + o)

renderColorChar :: Color -> Char -> Doc Style
renderColorChar clr ch = annotate (Ansi $ color clr) $ pretty ch

renderDisplay :: SimpleDocStream Style -> Text
renderDisplay = renderSdt . treeForm

renderSdt :: SimpleDocTree Style -> Text
renderSdt sdt = case sdt of
  STEmpty -> mempty
  STChar c -> T.singleton c
  STText _ t -> T.fromStrict t
  STLine i -> T.singleton '\n' <> T.replicate (fromIntegral i) (T.singleton ' ')
  STAnn ann rest -> renderStyle ann rest $ renderSdt rest
  STConcat xs -> foldMap renderSdt xs

renderStyle :: Style -> SimpleDocTree Style -> Text -> Text
renderStyle d _ s = case d of
  Ansi a -> go $ annotate a $ pretty s
  Title  -> go $ annotate (bold <> underlined <> color Blue) $ pretty s
  where
    go = renderLazy . layoutPretty defaultLayoutOptions

data Style
  = Ansi AnsiStyle
  | Title
  deriving (Eq, Show)
