module Berks.Bars.Rena
  ( renasBars,
  )
where

import Berks.Bars.Utils
  ( createBarConfigs,
    createBarStrut,
  )
import Berks.Widgets.Battery (batteryWidget)
import Berks.Widgets.CPU (cpuWidget)
import Berks.Widgets.Clock
  ( calendarWidget,
    clockWidget,
  )
import Berks.Widgets.Crypto
  ( btcWidget,
    ethWidget,
  )
import Berks.Widgets.Divider (plainDividerWidget)
import Berks.Widgets.FSMonitor (fsMonitorWidget)
import Berks.Widgets.Layout (layoutWidget)
import Berks.Widgets.Memory (memoryWidget)
import Berks.Widgets.MultiCoreTemp (cpuTempWidget)
import Berks.Widgets.Ping (connectivityWidget)
import Berks.Widgets.PowerMenu (powerMenuButton)
import Berks.Widgets.SniTray (sniTrayWidget)
import Berks.Widgets.Wakatime (wakatimeWidget)
import Berks.Widgets.WindowsWidget (windowsWidget)
import Berks.Widgets.Workspaces (workspacesWidget)
import Data.List (intersperse)
import Data.Unique (Unique)
import Graphics.UI.GIGtkStrut (StrutPosition (..))
import System.Taffybar.Context (BarConfig (..))

createPrimary :: Unique -> BarConfig
createPrimary barId' =
  BarConfig
    { strutConfig = createBarStrut 0 TopPos $ Just 45,
      barId = barId',
      widgetSpacing = 0,
      startWidgets = [workspacesWidget],
      centerWidgets =
        intersperse
          plainDividerWidget
          [clockWidget, layoutWidget, windowsWidget],
      endWidgets =
        reverse
          [ powerMenuButton,
            plainDividerWidget,
            cpuTempWidget,
            plainDividerWidget,
            fsMonitorWidget,
            cpuWidget,
            memoryWidget,
            sniTrayWidget
          ]
    }

createSecondary :: Unique -> BarConfig
createSecondary barId' =
  BarConfig
    { strutConfig = createBarStrut 0 BottomPos $ Just 45,
      barId = barId',
      widgetSpacing = 0,
      startWidgets = [calendarWidget],
      centerWidgets = [wakatimeWidget, plainDividerWidget, ethWidget, btcWidget],
      endWidgets =
        reverse
          [ batteryWidget,
            plainDividerWidget,
            connectivityWidget
          ]
    }

renasBars :: IO [BarConfig]
renasBars = createBarConfigs [createPrimary, createSecondary]
