module Berks.Widgets.CPU
  ( cpuWidget,
  )
where

import Berks.Colors
import Berks.WidgetUtils
  ( decorateWithClassname,
    myDefaultGraphConfig,
  )
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

cpuWidget :: MonadIO m => m Widget
cpuWidget =
  decorateWithClassname "graph" $
    pollingGraphNew
      myDefaultGraphConfig
        { graphDataColors = [red rgbas, lavender rgbas],
          graphLabel = Just "<span fgcolor='cyan'>\xf0ee0</span>"
        }
      0.5
      cpuCallback
