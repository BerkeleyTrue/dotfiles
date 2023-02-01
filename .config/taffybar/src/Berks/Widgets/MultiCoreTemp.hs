module Berks.Widgets.MultiCoreTemp
  ( cpuTempWidget,
  )
where

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
import Text.Printf (printf)

cpuTempWidget :: MonadIO m => m Widget
cpuTempWidget =
  pollingLabelNew 1 $
    getMultiCoreTemps
      <&> pack
        . (\temp -> "\63687: " ++ temp ++ "°C")
        . printf "%.0f"
