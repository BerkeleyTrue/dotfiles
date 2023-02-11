module Berks.Bars.Rena
  ( renasBars,
  )
where

import Berks.Bars.Utils
  ( createBarConfigs,
    createBarStrut,
  )
import Berks.Widgets.CPU (cpuWidget)
import Berks.Widgets.Clock
  ( calendarWidget,
    clockWidget,
  )
import Berks.Widgets.Crypto
  ( btcWidget,
    ethWidget,
    ohmWidget,
    pickleWidget,
    spyWidget,
    xtzWidget,
  )
import Berks.Widgets.Divider (plainDividerWidget)
import Berks.Widgets.FSMonitor (fsMonitorWidget)
import Berks.Widgets.Layout (layoutWidget)
import Berks.Widgets.Memory (memoryWidget)
import Berks.Widgets.MultiCoreTemp (cpuTempWidget)
import Berks.Widgets.PicomSwitch (picomSwitchWidget)
import Berks.Widgets.PowerMenu (powerMenuButton)
import Berks.Widgets.SniTray (sniTrayWidget)
import Berks.Widgets.Wakatime (wakatimeWidget)
import Berks.Widgets.Weather (weatherWidget)
import Berks.Widgets.WindowsWidget (windowsWidget)
import Berks.Widgets.Workspaces (workspacesWidget)
import Data.List (intersperse)
import Data.Unique (Unique)
import Graphics.UI.GIGtkStrut (StrutPosition (..))
import System.Taffybar.Context (BarConfig (..))

createPrimary :: Unique -> BarConfig
createPrimary barId' =
  BarConfig
    { strutConfig = createBarStrut 0 TopPos,
      barId = barId',
      widgetSpacing = 0,
      startWidgets = [workspacesWidget],
      centerWidgets =
        intersperse
          plainDividerWidget
          [clockWidget, layoutWidget, windowsWidget, wakatimeWidget],
      endWidgets =
        -- TODO: Add battery
        reverse
          [ picomSwitchWidget,
            plainDividerWidget,
            powerMenuButton,
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
    { strutConfig = createBarStrut 0 BottomPos,
      barId = barId',
      widgetSpacing = 0,
      startWidgets = [calendarWidget],
      centerWidgets = [ethWidget, weatherWidget],
      endWidgets =
        reverse
          [btcWidget, ohmWidget, xtzWidget, pickleWidget, spyWidget]
    }

renasBars :: IO [BarConfig]
renasBars = createBarConfigs [createPrimary, createSecondary]
