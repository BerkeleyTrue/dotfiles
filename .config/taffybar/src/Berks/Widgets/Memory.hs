module Berks.Widgets.Memory
  ( memoryWidget,
  )
where

import Berks.Colors as Colors
import Berks.WidgetUtils (decorateWithClassname)
import Control.Monad.IO.Class (MonadIO)
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

memCfg :: GraphConfig -> GraphConfig
memCfg cfg =
  cfg
    { graphDataColors = [Colors.yellow],
      graphLabel = Just "<span size='small' fgcolor='#ff5555'>\xf233</span>"
    }

memoryWidget :: MonadIO m => GraphConfig -> m Widget
memoryWidget cfg =
  decorateWithClassname "graph" $
    pollingGraphNew (memCfg cfg) 0.5 memCallback
