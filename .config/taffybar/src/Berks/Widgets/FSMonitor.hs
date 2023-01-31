module Berks.Widgets.FSMonitor
  ( fsMonitorWidget,
    showFSInfo,
  )
where

import Berks.WidgetUtils (setWidgetClassnameFromString)
import Control.Monad.IO.Class (MonadIO)
import Data.Text as T
  ( append,
    pack,
  )
import GI.Gtk as Gtk
  ( Widget,
  )
import System.Process
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )

showFSInfo :: [String] -> IO String
showFSInfo fsList =
  head -- take only the first item
    . take 1 -- take the first line
    . map (last . take 3 . reverse . words) -- get the GB of free space
    . drop 1 -- drop the headers
    . lines -- split the output into lines
    <$> readProcess "df" ("-kPH" : fsList) ""

fsMonitorWidget :: MonadIO m => m Widget
fsMonitorWidget =
  setWidgetClassnameFromString "fs-monitor"
    =<< pollingLabelNew
      1
      ((append . pack) "\xf0c7 " . pack <$> showFSInfo ["/"])
