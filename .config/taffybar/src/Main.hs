{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Default (def)
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO ()
main = do
  let cpuCfg =
        def
          { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)],
            graphLabel = Just "cpu"
          }
      clock = textClockNewWith cfg
        where
          cfg =
            def
              { clockFormatString =
                  "<span fgcolor='cyan'> \xf073 %a %b %d | Week %V \988226 %H:%M:%S</span>"
              }
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      workspaces = workspacesNew def
      simpleConfig =
        def
          { startWidgets = [workspaces],
            centerWidgets = [clock],
            endWidgets = [cpu, sniTrayNew],
            barPosition = Bottom
          }
  simpleTaffybar simpleConfig
