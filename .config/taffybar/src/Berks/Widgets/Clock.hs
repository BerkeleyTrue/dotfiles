module Berks.Widgets.Clock
  ( clockWidget,
  )
where

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
  def
    { clockFormatString =
        "<span fgcolor='cyan'> \xf073 %a %b %d | Week %V \988226 %H:%M:%S</span>"
    }

clockWidget :: MonadIO m => m Widget
clockWidget = decorateWithClassname "clock" $ textClockNewWith clockConfig
