module Berks.Widgets.WindowsWidget
  ( windowsWidget,
  )
where

import Berks.Colors (hexes, C (..))
import Data.Text as T
  ( pack,
    unpack,
  )
import GI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Util (truncateString)
import System.Taffybar.Widget.Util (colorize)
import System.Taffybar.Widget.Windows
  ( WindowsConfig (..),
    defaultGetActiveLabel,
    defaultWindowsConfig,
    windowsNew,
  )

defaultLabel :: String -> String
defaultLabel [] = "()"
defaultLabel x = x

windowsWidget :: TaffyIO Widget
windowsWidget =
  windowsNew
    defaultWindowsConfig
      { getActiveLabel = getActiveLabel'
      }
  where
    getActiveLabel' =
      pack
        . colorize (mauve hexes) ""
        . defaultLabel
        . truncateString 20
        . unpack
        <$> defaultGetActiveLabel
