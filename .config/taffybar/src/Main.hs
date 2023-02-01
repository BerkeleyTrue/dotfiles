module Main
  ( main,
  )
where

import Berks.Colors as Colors
import Berks.Widgets.CPU (cpuWidget)
import Berks.Widgets.Clock (clockWidget)
import Berks.Widgets.FSMonitor (fsMonitorWidget)
import Berks.Widgets.MultiCoreTemp (cpuTempWidget)
import Data.Default (def)
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import Berks.Widgets.Memory (memoryWidget)

myDefaultGraphConfig :: GraphConfig
myDefaultGraphConfig =
  defaultGraphConfig
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 25,
      graphBackgroundColor = Colors.transparent
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
  let workspaces = workspacesNew workspaceConfig
      simpleConfig =
        def
          { startWidgets = [workspaces],
            centerWidgets = [clockWidget],
            endWidgets =
              reverse
                [cpuTempWidget, fsMonitorWidget, cpuWidget myDefaultGraphConfig, memoryWidget myDefaultGraphConfig],
            barPosition = Bottom,
            monitorsAction = usePrimaryMonitor,
            barPadding = 0,
            barHeight = ExactSize 30
          }
  simpleTaffybar simpleConfig
