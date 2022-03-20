module Berks.Scratchpads
  ( myScratchPads
  , scratchpadManageHook
  , getAction
  ) where

import XMonad
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import qualified Berks.Utils as Utils
import qualified XMonad.StackSet as W

myScratchPads :: NamedScratchpads
myScratchPads = [NS "music" spawnMusic findMusic manageMusic]
  where
    spawnMusic = "chromium --app='https://music.youtube.com' --class='music'"
    findMusic = className =? "music"
    manageMusic = customFloating Utils.centerWindow

scratchpadManageHook :: ManageHook
scratchpadManageHook = namedScratchpadManageHook myScratchPads

getAction = namedScratchpadAction myScratchPads
