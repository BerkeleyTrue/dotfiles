module Berks.Widgets.Battery (batteryWidget) where

import Berks.Colors (C (..), hexes)
import Berks.WidgetUtils (decorateWithClassname)
import Data.Text (pack)
import GI.Gtk (Widget, labelSetMarkup)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Battery (BatteryInfo (batteryPercentage), BatteryState (..), batteryState)
import System.Taffybar.Widget.Battery (textBatteryNewWithLabelAction)
import System.Taffybar.Widget.Util (colorize)

-- get battery icon from percentage
getBatteryIcon :: Int -> String
getBatteryIcon x = case x of
  _ | x > 90 -> "\xf0082" -- 󰂂
  _ | x > 80 -> "\xf0081" -- 󰂁
  _ | x > 70 -> "\xf0080" -- 󰂀
  _ | x > 60 -> "\xf007f" -- 󰁿
  _ | x > 50 -> "\xf007e" -- 󰁾
  _ | x > 40 -> "\xf007d" -- 󰁽
  _ | x > 30 -> "\xf007c" -- 󰁼
  _ | x > 20 -> "\xf007b" -- 󰁻
  _ | x > 10 -> "\xf007a" -- 󰁺
  _ -> "\xf0079" -- 󰁹

getBatteryColor :: Int -> String
getBatteryColor x = case x of
  _ | x > 70 -> green hexes
  _ | x > 40 -> yellow hexes
  _ -> red hexes

batteryWidget :: TaffyIO Widget
batteryWidget =
  decorateWithClassname "battery" $
    textBatteryNewWithLabelAction labelSetter
  where
    labelSetter label info = do
      let percent = floor $ batteryPercentage info
      let percentShow = show percent
      let color = case batteryState info of
            BatteryStateCharging -> green hexes
            BatteryStateFullyCharged -> green hexes
            BatteryStateDischarging -> getBatteryColor percent
            BatteryStateEmpty -> red hexes
            _ -> red hexes

      let icon = case batteryState info of
            BatteryStateCharging -> "\xf0084" -- 󰂄
            BatteryStateFullyCharged -> "\xf0079" -- 󰁹
            BatteryStateDischarging -> getBatteryIcon percent
            BatteryStateEmpty -> "\xf007a" -- 󰁺
            _ -> "\xf0083" -- 󰂃
      labelSetMarkup label $ pack $ colorize color "" $ icon ++ " " ++ percentShow ++ "%"
