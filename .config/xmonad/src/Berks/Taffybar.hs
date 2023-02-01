module Berks.Taffybar
  ( startTaffybar,
    rebuildTaffybar,
  )
where

import XMonad.Core

taffyRestartCmd :: String
taffyRestartCmd =
  "killall bottom-taffybar &> /dev/null"

rebuildTaffybar :: X ()
rebuildTaffybar =
  spawn $ "notify-send -a 'XMonad' 'Rebuilding Taffybar' && $HOME/.config/taffybar/compile && " ++ taffyRestartCmd

startTaffybar :: X ()
startTaffybar = spawn "systemctl start --user taffybar"
