{-# LANGUAGE OverloadedStrings #-}
module Backend.Graphs where

-- Temporary graph rendering
import Control.Lens ((.~))
import Control.Monad (forM, guard)
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import Data.Colour (opaque)
import Data.Colour.SRGB (sRGB)
import Data.Default (def)
import Data.Fixed (Micro)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word64)
import Diagrams.Backend.SVG (Options (..), SVG (..))
import Diagrams.Core (renderDia)
import Diagrams.TwoD.Size (mkWidth)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import qualified Graphics.Svg.Core as SVG (renderText)
import Rhyolite.Schema (Id)

import Common.Schema

cumulativeRewardsGraph :: Word64 -> AppendMap (Id Client) (AppendMap Word64 Micro) -> IO (Maybe (Micro, Text))
cumulativeRewardsGraph level rewards = cumulativeRewardsGraph' (fromIntegral level) (fmap (Map.mapKeys fromIntegral) rewards)

cumulativeRewardsGraph' :: Integer -> AppendMap (Id Client) (AppendMap Integer Micro) -> IO (Maybe (Micro, Text))
cumulativeRewardsGraph' level rewards = do
  let totalRewards :: AppendMap Integer Micro
      totalRewards = foldl' (Map.unionWith (+)) Map.empty rewards

      cumulate :: (Ord a, Integral a, Num b) => a -> b -> AppendMap a b -> [(a,b)]
      cumulate l p m = foldr (\(x,y) xs s -> let y' = s + y in (x - l, s) : (x - l, y') : xs y') (\_ -> []) (Map.toList m) p

      cumulativeRewards :: Maybe (Micro, [(Integer, Micro)])
      cumulativeRewards = do
        let (before, x, after) = Map.splitLookup (level - 200) totalRewards -- NB: we need to push this splitting into the backend
            principal = sum before + fromMaybe 0 x -- just in the sense of where the graph starts
            (past, x', _future) = Map.splitLookup level after
            total = principal + sum past + fromMaybe 0 x'
            graphPoints = cumulate level principal after
        guard (not . null . drop 2 $ graphPoints) -- if there's too little data, don't bother producing a shitty graph
        return (total, cumulate level principal after)
  forM cumulativeRewards $ \(mtz, xs) -> do
    graph <- renderGraph "Cumulative Rewards" xs
    return (mtz, graph)

renderGraph :: (Integral a, Real b) => Text -> [(a,b)] -> IO Text
renderGraph t xs = do
  let chart = toRenderable layout
      plot1 = plot_lines_style . line_color .~ opaque (sRGB 0.1 0.5 0.1)
            $ plot_lines_values .~ [[(fromIntegral l :: Integer,realToFrac x :: Double) | (l,x) <- xs]]
            $ def
      layout = layout_title .~ T.unpack t
             $ layout_plots .~ [toPlot plot1]
             $ def
  env <- defaultEnv vectorAlignmentFns 300 300
  let (diagram, _) = runBackendR env chart
      svgOptions = SVGOptions
        { _size = mkWidth 250
        , _svgDefinitions = Nothing
        , _idPrefix = ""
        , _svgAttributes = []
        , _generateDoctype = False
        }
  return (TL.toStrict (SVG.renderText (renderDia SVG svgOptions diagram)))
