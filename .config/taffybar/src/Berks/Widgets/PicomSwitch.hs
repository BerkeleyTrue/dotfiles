module Berks.Widgets.PicomSwitch
  ( picomSwitchWidget,
  )
where

import Berks.Colors (sapphire, hexes)
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
      "off\n" -> runCommand "systemctl" ["--user", "start", "picom.service"]
      "on\n" -> runCommand "systemctl" ["--user", "stop", "picom.service"]
      _ -> runCommand "systemctl" ["--user", "stop", "picom.service"]

  return ()

picomLabel :: IO String
picomLabel = do
  cmdEither <- runCommand "is-picom-on" []
  label' <- case cmdEither of
    Left _ -> return "\xf1e2"
    Right res -> case res of
      "off\n" -> return "\xf204"
      "on\n" -> return "\xf205"
      _ -> return "\xf1e2"

  return $ colorize (sapphire hexes) "" label'

picomSwitchWidget :: TaffyIO Widget
picomSwitchWidget =
  pollingLabelButtonNew 2 picomLabel $ const picomToggleCommand
