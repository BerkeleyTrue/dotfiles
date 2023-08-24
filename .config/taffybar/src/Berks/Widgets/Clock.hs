module Berks.Widgets.Clock
  ( clockWidget,
    calendarWidget,
  )
where

import Berks.Colors
import Berks.WidgetUtils (decorateWithClassname)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default (def))
import GI.Gtk (Widget)
import System.Taffybar.Widget
  ( ClockConfig (clockFormatString),
    textClockNewWith,
  )

clockConfig :: ClockConfig
clockConfig =
  def {clockFormatString = "<span fgcolor='" <> lavender hexes <> "'>\988226 %H:%M:%S</span>"}

calendarConfig :: ClockConfig
calendarConfig =
  def
    { clockFormatString = "<span fgcolor='" <> lavender hexes <> "'> \xf073 %a %b %d | Week %V</span>"
    }

clockWidget :: (MonadIO m) => m Widget
clockWidget = decorateWithClassname "clock" $ textClockNewWith clockConfig

calendarWidget :: (MonadIO m) => m Widget
calendarWidget = decorateWithClassname "calendar" $ textClockNewWith calendarConfig
