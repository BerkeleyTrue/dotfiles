module Berks.Bars.Delora
  ( delorasBars,
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
import Data.Int (Int32)
import Data.List (intersperse)
import Data.Unique (Unique)
import Graphics.UI.GIGtkStrut (StrutPosition (..))
import System.Taffybar.Context (BarConfig (..))

createPrimary :: Int32 -> Unique -> BarConfig
createPrimary monitor barId' =
  BarConfig
    { strutConfig = createBarStrut monitor TopPos Nothing,
      barId = barId',
      widgetSpacing = 0,
      startWidgets = [workspacesWidget, calendarWidget],
      centerWidgets =
        intersperse
          plainDividerWidget
          [ clockWidget,
            layoutWidget,
            windowsWidget,
            wakatimeWidget,
            ethWidget
          ],
      endWidgets =
        reverse
          [ powerMenuButton,
            plainDividerWidget,
            connectivityWidget,
            plainDividerWidget,
            cpuTempWidget,
            plainDividerWidget,
            fsMonitorWidget,
            plainDividerWidget,
            cpuWidget,
            memoryWidget,
            sniTrayWidget
          ]
    }

createSecondary :: Int32 -> Unique -> BarConfig
createSecondary monitor barId' =
  BarConfig
    { strutConfig = createBarStrut monitor BottomPos Nothing,
      barId = barId',
      widgetSpacing = 0,
      startWidgets = [workspacesWidget],
      centerWidgets = intersperse plainDividerWidget [calendarWidget, clockWidget],
      endWidgets =
        reverse
          [btcWidget]
    }

delorasBars :: Int32 -> IO [BarConfig]
delorasBars monitors =
  if monitors >= 2
    then createBarConfigs [createPrimary 0, createSecondary 1]
    else createBarConfigs [createPrimary 0, createSecondary 0]
