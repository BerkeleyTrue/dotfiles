module Berks.Scratchpads
  ( myScratchPads
  , scratchpadManageHook
  , getAction
  ) where

import XMonad
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W

center :: Rational -> Rational
center ratio = (1 - ratio) / 2

myScratchPads :: NamedScratchpads
myScratchPads = [NS "music" spawnMusic findMusic manageMusic]
  where
    spawnMusic = "chromium --app='https://music.youtube.com' --class='music'"
    findMusic = className =? "music"
    manageMusic = customFloating $ W.RationalRect x y w h
      where
        w = 2 / 5
        h = 3 / 5
        x = center w
        y = center h

scratchpadManageHook :: ManageHook
scratchpadManageHook = namedScratchpadManageHook myScratchPads

getAction = namedScratchpadAction myScratchPads
