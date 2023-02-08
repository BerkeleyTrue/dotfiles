module Berks.Widgets.Wakatime
  ( wakatimeWidget,
  )
where

import Berks.Colors (greenHex)
import Berks.WidgetUtils (runCommandWithDefault)
import Data.Text
  ( Text,
    pack,
  )
import GI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )
import System.Taffybar.Widget.Util (colorize)

wakaCommand :: IO Text
wakaCommand =
  pack . colorize greenHex "" . ("\xe000 " <>)
    <$> runCommandWithDefault
      "wakatime"
      ["--today"]
      "x"

wakatimeWidget :: TaffyIO Widget
wakatimeWidget = pollingLabelNew 2 wakaCommand
