module Berks.Widgets.WindowsWidget
  ( windowsWidget,
  )
where

import GI.Gtk (Widget)
import System.Taffybar.Widget.Windows
  ( WindowsConfig (..),
    defaultGetActiveLabel,
    defaultWindowsConfig,
    windowsNew,
  )
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Util (truncateText)
import System.Taffybar.Widget.Util (colorize)
import Data.Text (pack, unpack)
import Berks.Colors (purpleHex)

windowsWidget :: TaffyIO Widget
windowsWidget =
  windowsNew
    defaultWindowsConfig
      { getActiveLabel = getActiveLabel'
      }
  where
    getActiveLabel' = (pack . colorize purpleHex "") . unpack . truncateText 20 <$> defaultGetActiveLabel
