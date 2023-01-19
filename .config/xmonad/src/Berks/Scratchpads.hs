module Berks.Scratchpads
  ( myScratchPads,
    scratchpadManageHook,
    getAction,
  )
where

import Berks.Utils
import XMonad
import XMonad.ManageHook
import XMonad.StackSet
import XMonad.Util.NamedScratchpad

myScratchPads :: NamedScratchpads
myScratchPads = [NS "music" spawnMusic findMusic manageMusic]
  where
    spawnMusic = "app.ytmdesktop.ytmdesktop"
    findMusic = className =? "youtube-music-desktop-app"
    manageMusic = customFloating centerWindow

scratchpadManageHook :: ManageHook
scratchpadManageHook = namedScratchpadManageHook myScratchPads

getAction = namedScratchpadAction myScratchPads
