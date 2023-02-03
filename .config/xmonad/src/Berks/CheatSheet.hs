module Berks.CheatSheet
  ( cheatSheetView,
    size,
  )
where

import Data.Ratio ((%))
import System.IO (hClose)
import XMonad
import XMonad.StackSet as W
  ( RationalRect (RationalRect),
  )
import XMonad.Util.NamedActions
import XMonad.Util.Run

-- Size and location of the popup
size :: RationalRect
size = RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)

-- How to display the cheatsheet (adapted from Ethan Schoonover's config)
cheatSheetView :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
cheatSheetView keyMap = addName "View Xmonad Keybindings" $
  io $ do
    handle <-
      spawnPipe
        "yad --no-buttons \
        \--fontname='FiraCode Nerd Font 14' \
        \--text-info"
    hPutStrLn handle $ unlines $ showKm keyMap
    hClose handle
    return ()
