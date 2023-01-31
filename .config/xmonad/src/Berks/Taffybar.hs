module Berks.Taffybar
  ( startTaffybar,
    rebuildTaffybar,
  )
where

import XMonad.Core

taffyRestartCmd :: String
taffyRestartCmd =
  "killall bottom-taffybar &> /dev/null; sleep 1 && bottom-taffybar &"

rebuildTaffybar :: X ()
rebuildTaffybar =
  spawn $ "notify-send -a 'XMonad' 'Rebuilding Taffybar' && $HOME/.config/taffybar/compile && " ++ taffyRestartCmd

startTaffybar :: X ()
startTaffybar = spawn taffyRestartCmd
