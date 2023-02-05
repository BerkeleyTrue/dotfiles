module Main
  ( main,
  )
where

import Berks.Colors as Colors
import Berks.Widgets.CPU (cpuWidget)
import Berks.Widgets.Clock (clockWidget)
import Berks.Widgets.Divider
  ( dividerWidget,
    plainDividerWidget,
  )
import Berks.Widgets.FSMonitor (fsMonitorWidget)
import Berks.Widgets.Layout (layoutWidget)
import Berks.Widgets.Memory (memoryWidget)
import Berks.Widgets.MultiCoreTemp (cpuTempWidget)
import Berks.Widgets.Weather (weatherWidget)
import Berks.Widgets.WindowsWidget (windowsWidget)
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
import System.Taffybar.Widget.SNITray

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
            centerWidgets =
              [ clockWidget,
                plainDividerWidget,
                layoutWidget,
                plainDividerWidget,
                windowsWidget,
                plainDividerWidget,
                weatherWidget
              ],
            endWidgets =
              reverse
                [ cpuTempWidget,
                  dividerWidget Nothing,
                  fsMonitorWidget,
                  cpuWidget myDefaultGraphConfig,
                  memoryWidget myDefaultGraphConfig,
                  sniTrayNew
                ],
            barPosition = Bottom,
            monitorsAction = usePrimaryMonitor,
            barPadding = 0,
            barHeight = ExactSize 30
          }
  simpleTaffybar simpleConfig
