{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Berks.Colors as Colors
import Berks.WidgetUtils
import Data.Default (def)
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph

myDefaultGraphConfig :: GraphConfig
myDefaultGraphConfig =
  defaultGraphConfig
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 50,
      graphBackgroundColor = Colors.selection
    }

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

cpuCfg :: GraphConfig
cpuCfg =
  myDefaultGraphConfig
    { graphDataColors = [Colors.red, Colors.cyan],
      graphLabel = Just "<span fgcolor='cyan'>\xf0aab</span>"
    }

clockConfig :: ClockConfig
clockConfig =
  def
    { clockFormatString =
        "<span fgcolor='cyan'> \xf073 %a %b %d | Week %V \988226 %H:%M:%S</span>"
    }

filterHiddenAndNSP :: Workspace -> Bool
filterHiddenAndNSP Workspace {workspaceState = Empty} = False
filterHiddenAndNSP Workspace {workspaceName = "NSP"} = False
filterHiddenAndNSP _ = True

workspaceConfig :: WorkspacesConfig
workspaceConfig =
  def {minIcons = 1, widgetGap = 1, showWorkspaceFn = filterHiddenAndNSP}

main :: IO ()
main = do
  let clock = deocrateWithClassname "clock" $ textClockNewWith clockConfig
      cpu =
        deocrateWithClassname "cpu" $ pollingGraphNew cpuCfg 0.5 cpuCallback
      workspaces = workspacesNew workspaceConfig
      simpleConfig =
        def
          { startWidgets = [workspaces],
            centerWidgets = [clock],
            endWidgets = [sniTrayNew, cpu],
            barPosition = Bottom,
            cssPaths = ["src/taffybar.css"],
            monitorsAction = usePrimaryMonitor,
            barPadding = 0,
            barHeight = ExactSize 30
          }
  simpleTaffybar simpleConfig
