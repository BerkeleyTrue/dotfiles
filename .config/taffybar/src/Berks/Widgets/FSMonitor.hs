module Berks.Widgets.FSMonitor
  ( fsMonitorWidget,
  )
where

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

fsMonitorWidget :: MonadIO m => m Widget
fsMonitorWidget =
  setWidgetClassnameFromString "fs-monitor"
    =<< pollingLabelNew 1 (("\xf0c7 " <>) . pack <$> showFSInfo ["/"])
