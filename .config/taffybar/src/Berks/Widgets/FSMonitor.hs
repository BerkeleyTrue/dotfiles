module Berks.Widgets.FSMonitor
  ( fsMonitorWidget,
  )
where

import Berks.Colors (commentHex)
import Berks.Information.FSMonitor (showFSInfo)
import Berks.WidgetUtils (setWidgetClassnameFromString)
import Control.Monad.IO.Class (MonadIO)
import Data.Text as T
  ( pack,
  )
import GI.Gtk as Gtk
  ( Widget,
  )
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )
import System.Taffybar.Widget.Util (colorize)

fsMonitorWidget :: MonadIO m => m Widget
fsMonitorWidget =
  setWidgetClassnameFromString "fs-monitor"
    =<< pollingLabelNew
      1
      (pack . colorize commentHex "" . ("\xf0c7 " <>) <$> showFSInfo ["/"])
