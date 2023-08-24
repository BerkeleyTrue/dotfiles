module Berks.Widgets.MultiCoreTemp
  ( cpuTempWidget,
  )
where

import Berks.Colors
  ( hexes,
    sapphire,
    peach,
    yellow,
  )
import Berks.Information.MultiCoreTemp
  ( getMultiCoreTemps,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Functor
import Data.Text (pack)
import GI.Gtk (Widget)
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )
import System.Taffybar.Widget.Util (colorize)
import Text.Printf (printf)

cpuTempWidget :: MonadIO m => m Widget
cpuTempWidget =
  pollingLabelNew 1 $
    getMultiCoreTemps
      <&> pack
        . ( \temp ->
              colorize
                (peach hexes)
                ""
                ("\xf03c8: " <> temp <> colorize (sapphire hexes) "" "°" <> "C")
          )
        . colorize (yellow hexes) ""
        . printf "%.0f"
