module Main
  ( main,
  )
where

import Berks.Colors as Colors
import Berks.Widgets.CPU (cpuWidget)
import Berks.Widgets.Clock (clockWidget)
import Berks.Widgets.FSMonitor (fsMonitorWidget)
import Berks.Widgets.Memory (memoryWidget)
import Berks.Widgets.MultiCoreTemp (cpuTempWidget)
import Berks.Widgets.Weather (weatherWidget)
import Berks.Widgets.Workspaces (workspacesWidget)
import Data.Default (def)
import System.Log.Logger
  ( Priority (..),
    getLogger,
    saveGlobalLogger,
    setLevel,
  )
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.Generic.Graph

myDefaultGraphConfig :: GraphConfig
myDefaultGraphConfig =
  defaultGraphConfig
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 25,
      graphBackgroundColor = Colors.transparent
    }

main :: IO ()
main = do
  logger <- getLogger "System.Taffybar.Widget.Weather"
  saveGlobalLogger $ setLevel ERROR logger
  let simpleConfig =
        def
          { startWidgets = [workspacesWidget],
            centerWidgets = [clockWidget, weatherWidget],
            endWidgets =
              reverse
                [ cpuTempWidget,
                  fsMonitorWidget,
                  cpuWidget myDefaultGraphConfig,
                  memoryWidget myDefaultGraphConfig
                ],
            barPosition = Bottom,
            monitorsAction = usePrimaryMonitor,
            barPadding = 0,
            barHeight = ExactSize 30
          }
  simpleTaffybar simpleConfig
