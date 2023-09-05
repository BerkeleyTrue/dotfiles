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
import Berks.Widgets.PowerMenu (powerMenuButton)
import Berks.Widgets.SniTray (sniTrayWidget)
import Berks.Widgets.Wakatime (wakatimeWidget)
import Berks.Widgets.Weather (weatherWidget)
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
            ethWidget,
            weatherWidget
          ],
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

createSecondary :: Int32 -> Unique -> BarConfig
createSecondary monitor barId' =
  BarConfig
    { strutConfig = createBarStrut monitor BottomPos Nothing,
      barId = barId',
      widgetSpacing = 0,
      startWidgets = [workspacesWidget],
      centerWidgets = [calendarWidget, plainDividerWidget, clockWidget],
      endWidgets =
        reverse
          [btcWidget, ohmWidget, xtzWidget, pickleWidget, spyWidget]
    }

delorasBars :: Int32 -> IO [BarConfig]
delorasBars monitors =
  if monitors >= 1
    then createBarConfigs [createPrimary monitors, createSecondary monitors]
    else createBarConfigs [createPrimary 0, createSecondary 0]
