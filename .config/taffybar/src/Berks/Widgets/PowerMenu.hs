module Berks.Widgets.PowerMenu
  ( powerMenuButton,
  )
where

import Berks.WidgetUtils (buttonWithClickHandler)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk (Widget)
import System.Taffybar.Util (runCommand)
import System.Taffybar.Widget.Util (colorize)
import Berks.Colors (redHex)


command :: IO ()
command = void $ runCommand "powermenu" []



powerMenuButton :: MonadIO m => m Widget
powerMenuButton =
  buttonWithClickHandler (colorize redHex "" "\x23fb") $ const command