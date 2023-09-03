module Berks.Widgets.Battery (batteryWidget) where

import Berks.WidgetUtils (decorateWithClassname)
import GI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Battery (textBatteryNew)

batteryWidget :: TaffyIO Widget
batteryWidget =
  decorateWithClassname "battery" $
    textBatteryNew "\xf0079 $percentage$%"
