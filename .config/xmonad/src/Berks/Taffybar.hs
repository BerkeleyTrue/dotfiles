module Berks.Taffybar
  ( startTaffybar,
    rebuildTaffybar,
  )
where

import XMonad.Core

taffyRestartCmd :: String
taffyRestartCmd =
  "systemctl --user restart taffybar.service"

rebuildTaffybar :: X ()
rebuildTaffybar =
  spawn $ "notify-send -a 'XMonad' 'Rebuilding Taffybar' && $HOME/.config/taffybar/compile && " ++ taffyRestartCmd

startTaffybar :: X ()
startTaffybar = spawn "sleep 1 && systemctl start --user taffybar"
