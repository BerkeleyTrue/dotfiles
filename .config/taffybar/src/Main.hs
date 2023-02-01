module Main
  ( main,
  )
where

import Berks.Colors as Colors
import Berks.MultiCoreTemp
import Berks.WidgetUtils
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Functor
import Data.Text hiding (reverse)
import GI.Gtk as Gtk
  ( Widget,
  )
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel
import Text.Printf
import Berks.Widgets.FSMonitor (fsMonitorWidget)

myDefaultGraphConfig :: GraphConfig
myDefaultGraphConfig =
  defaultGraphConfig
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 25,
      graphBackgroundColor = Colors.transparent
    }

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

cpuCfg :: GraphConfig
cpuCfg =
  myDefaultGraphConfig
    { graphDataColors = [Colors.red, Colors.cyan],
      graphLabel = Just "<span fgcolor='cyan'>\xf0ee0</span>"
    }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

memCfg :: GraphConfig
memCfg =
  myDefaultGraphConfig
    { graphDataColors = [Colors.yellow],
      graphLabel = Just "<span size='small' fgcolor='#ff5555'>\xf233</span>"
    }

clockConfig :: ClockConfig
clockConfig =
  def
    { clockFormatString =
        "<span fgcolor='cyan'> \xf073 %a %b %d | Week %V \988226 %H:%M:%S</span>"
    }

cpuTemp :: MonadIO m => m Widget
cpuTemp =
  pollingLabelNew 1 $
    getMultiCoreTemps
      <&> pack
        . (\temp -> "\63687: " ++ temp ++ "°C")
        . printf "%.0f"

filterHiddenAndNSP :: Workspace -> Bool
filterHiddenAndNSP Workspace {workspaceState = Empty} = False
filterHiddenAndNSP Workspace {workspaceName = "NSP"} = False
filterHiddenAndNSP _ = True

workspaceConfig :: WorkspacesConfig
workspaceConfig =
  def {minIcons = 1, widgetGap = 1, showWorkspaceFn = filterHiddenAndNSP}

main :: IO ()
main = do
  let clock = decorateWithClassname "clock" $ textClockNewWith clockConfig
      cpu =
        decorateWithClassname "graph" $ pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = decorateWithClassname "graph" $ pollingGraphNew memCfg 5 memCallback
      workspaces = workspacesNew workspaceConfig
      simpleConfig =
        def
          { startWidgets = [workspaces],
            centerWidgets = [clock],
            endWidgets = reverse [cpuTemp, fsMonitorWidget, cpu, mem],
            barPosition = Bottom,
            monitorsAction = usePrimaryMonitor,
            barPadding = 0,
            barHeight = ExactSize 30
          }
  simpleTaffybar simpleConfig
