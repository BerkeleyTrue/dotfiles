module Berks.Widgets.CPU
  ( cpuWidget,
  )
where

import Berks.Colors as Colors
import Berks.WidgetUtils (decorateWithClassname)
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk (Widget)
import System.Taffybar.Information.CPU
  ( cpuLoad,
  )
import System.Taffybar.Widget.Generic.PollingGraph
  ( GraphConfig
      ( graphDataColors,
        graphLabel
      ),
    pollingGraphNew,
  )

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

cpuWidget :: MonadIO m => GraphConfig -> m Widget
cpuWidget cfg =
  decorateWithClassname "graph" $
    pollingGraphNew
      cfg
        { graphDataColors = [Colors.red, Colors.cyan],
          graphLabel = Just "<span fgcolor='cyan'>\xf0ee0</span>"
        }
      0.5
      cpuCallback