{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Default (def)
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph
import qualified GI.Gtk as Gtk

setClassAndBoundingBoxes :: MonadIO m => Data.Text.Text -> Gtk.Widget -> m Gtk.Widget
setClassAndBoundingBoxes className = buildContentsBox >=> flip widgetSetClassGI className

deocrateWithSetClassAndBoxes :: MonadIO m => Data.Text.Text -> m Gtk.Widget -> m Gtk.Widget
deocrateWithSetClassAndBoxes className builder = builder >>= setClassAndBoundingBoxes className

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

cpuCfg :: GraphConfig
cpuCfg =
  def
    { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
      graphLabel = Just "<span fgcolor='cyan'>\xf0aab</span>"
    }

clockConfig :: ClockConfig
clockConfig =
  def
    { clockFormatString =
        "<span fgcolor='cyan'> \xf073 %a %b %d | Week %V \988226 %H:%M:%S</span>"
    }

filterHiddenAndNSP :: Workspace -> Bool
filterHiddenAndNSP Workspace { workspaceState = Empty } = False
filterHiddenAndNSP Workspace { workspaceName = "NSP" } = False
filterHiddenAndNSP _ = True

workspaceConfig :: WorkspacesConfig
workspaceConfig =
  def {minIcons = 1, widgetGap = 1, showWorkspaceFn = filterHiddenAndNSP}


main :: IO ()
main = do
  let clock = deocrateWithSetClassAndBoxes "clock" $ textClockNewWith clockConfig
      cpu = deocrateWithSetClassAndBoxes "cpu" $ pollingGraphNew cpuCfg 0.5 cpuCallback
      workspaces = workspacesNew workspaceConfig
      simpleConfig =
        def
          { startWidgets = [workspaces],
            centerWidgets = [clock],
            endWidgets = [cpu, sniTrayNew],
            barPosition = Bottom,
            cssPaths = ["src/taffybar.css"],
            monitorsAction = usePrimaryMonitor,
            barPadding = 0,
            barHeight = ExactSize 30
          }
  simpleTaffybar simpleConfig
