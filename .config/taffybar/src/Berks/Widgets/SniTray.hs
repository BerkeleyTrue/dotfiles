module Berks.Widgets.SniTray
  ( sniTrayWidget,
  )
where

import GI.Gtk (Widget)
import StatusNotifier.Tray
  ( TrayImageSize (TrayImageSize),
    TrayParams (..),
    defaultTrayParams,
  )
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.SNITray (sniTrayNewFromParams)

sniTrayWidget :: TaffyIO Widget
sniTrayWidget =
  sniTrayNewFromParams $ defaultTrayParams {trayImageSize = TrayImageSize 16}
