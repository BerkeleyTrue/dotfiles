module Main
  ( main,
  )
where

import Berks.Colors as Colors
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
import Control.Monad.Trans.Class (lift)
import Data.Int (Int32)
import Data.List (intersperse)
import Data.Unique
  ( Unique,
    newUnique,
  )
import Graphics.UI.GIGtkStrut
import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..),
    TaffybarConfig (..),
  )
-- import System.Log.Logger
--   ( Priority (..),
--     getLogger,
--     saveGlobalLogger,
--     setLevel,
--   )
import System.Taffybar.Widget.Generic.Graph

myDefaultGraphConfig :: GraphConfig
myDefaultGraphConfig =
  defaultGraphConfig
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 25,
      graphBackgroundColor = Colors.transparent
    }

barStrut :: Int32 -> StrutPosition -> StrutConfig
barStrut monitor pos =
  StrutConfig
    { strutHeight = ExactSize 30,
      strutWidth = ScreenRatio 1,
      strutXPadding = 0,
      strutYPadding = 0,
      strutAlignment = Beginning,
      strutPosition = pos,
      strutMonitor = Just monitor,
      strutDisplayName = Nothing
    }

firstBarConfig :: Unique -> BarConfig
firstBarConfig barId =
  BarConfig
    { strutConfig = barStrut 0 TopPos,
      barId = barId,
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
          [ picomSwitchWidget,
            plainDividerWidget,
            powerMenuButton,
            plainDividerWidget,
            cpuTempWidget,
            plainDividerWidget,
            fsMonitorWidget,
            cpuWidget myDefaultGraphConfig,
            memoryWidget myDefaultGraphConfig,
            sniTrayWidget
          ]
    }

secondBarConfig :: Unique -> BarConfig
secondBarConfig barId =
  BarConfig
    { strutConfig = barStrut 1 TopPos,
      barId = barId,
      widgetSpacing = 0,
      startWidgets = [workspacesWidget],
      centerWidgets = [calendarWidget, plainDividerWidget, clockWidget],
      endWidgets =
        reverse
          [btcWidget, ohmWidget, xtzWidget, pickleWidget, spyWidget]
    }

withBarId :: (Unique -> BarConfig) -> IO BarConfig
withBarId f = f <$> newUnique

getBars :: IO [BarConfig]
getBars = do
  topBar <- withBarId firstBarConfig
  bottomBar <- withBarId secondBarConfig
  return [topBar, bottomBar]

main :: IO ()
main = do
  -- logger <- getLogger "Berks.WidgetUtils"
  -- logger2 <- getLogger "Berks.WidgetUtils"
  -- saveGlobalLogger $ setLevel INFO logger
  -- saveGlobalLogger $ setLevel DEBUG logger2
  startTaffybar $
    TaffybarConfig
      { dbusClientParam = Nothing,
        startupHook = return (),
        getBarConfigsParam = lift getBars,
        cssPaths = [],
        errorMsg = Nothing
      }
