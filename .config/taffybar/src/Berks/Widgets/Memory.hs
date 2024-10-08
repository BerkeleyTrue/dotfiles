module Berks.Widgets.Memory
  ( memoryWidget,
  )
where

import Berks.Colors
import Berks.WidgetUtils
  ( decorateWithClassname,
    myDefaultGraphConfig,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (pack)
import GI.Gtk (Widget)
import System.Taffybar.Information.Memory
  ( MemoryInfo (memoryUsedRatio),
    parseMeminfo,
  )
import System.Taffybar.Widget.Generic.Graph
  ( GraphConfig
      ( graphDataColors,
        graphLabel
      ),
  )
import System.Taffybar.Widget.Generic.PollingGraph
  ( pollingGraphNew,
  )

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

memCfg :: GraphConfig
memCfg =
  myDefaultGraphConfig
    { graphDataColors = [yellow rgbas],
      graphLabel = Just $ pack $ "<span size='small' fgcolor='" <> red hexes <> "'>\xf233</span>"
    }

memoryWidget :: (MonadIO m) => m Widget
memoryWidget =
  decorateWithClassname "graph" $ pollingGraphNew memCfg 0.5 memCallback
