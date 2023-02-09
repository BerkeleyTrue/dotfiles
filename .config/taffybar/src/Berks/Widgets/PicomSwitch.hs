module Berks.Widgets.PicomSwitch
  ( picomSwitchWidget,
  )
where

import Berks.Colors (cyanHex)
import Berks.WidgetUtils (pollingLabelButtonNew)
import GI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Util (runCommand)
import System.Taffybar.Widget.Util (colorize)

picomToggleCommand :: IO ()
picomToggleCommand = do
  isPicomOn <- runCommand "is-picom-on" []
  _ <- case isPicomOn of
    Left _ -> runCommand "systemctl" ["--user", "stop", "picom.service"]
    Right res -> case res of
      "true" -> runCommand "systemctl" ["--user", "stop", "picom.service"]
      "false" -> runCommand "systemctl" ["--user", "start", "picom.service"]
      _ -> runCommand "systemctl" ["--user", "stop", "picom.service"]

  return ()

picomLabel :: IO String
picomLabel = do
  cmdEither <- runCommand "is-picom-on" []
  label' <- case cmdEither of
    Left _ -> return "\xf204"
    Right _ -> return "\xf205"

  return $ colorize cyanHex "" label'

picomSwitchWidget :: TaffyIO Widget
picomSwitchWidget =
  pollingLabelButtonNew 2 picomLabel $ const picomToggleCommand
