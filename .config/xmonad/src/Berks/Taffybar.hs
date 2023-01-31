module Berks.Taffybar
  ( startTaffybar,
  )
where

import XMonad.Core

startTaffybar :: X ()
startTaffybar =
  spawn "killall bottom-taffybar &> /dev/null; sleep 1 && bottom-taffybar &"
