{-# OPTIONS_GHC -Wall #-}

module Berks.Layouts.Main
  ( layout,
  )
where

import Berks.Layouts.Monocle
import Berks.Layouts.ThreeCol
import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ScreenCorners
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Reflect
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

layout =
  ( screenCornerLayoutHook
      . mouseResize
      . windowArrange
      . windowNavigation
      . avoidStruts
  )
    $ mkToggle (single REFLECTY)
    $ mkToggle (single REFLECTX)
    $ mkToggle (single NOBORDERS)
    $ mkToggle (single FULL) $
      ifWider 2300 (threeCol ||| monocle) (monocle ||| threeCol)
