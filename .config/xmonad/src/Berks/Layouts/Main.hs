{-# OPTIONS_GHC -Wall #-}

module Berks.Layouts.Main
  ( layout,
  )
where

import Berks.Layouts.Horiz
import Berks.Layouts.Monocle as Mono
import Berks.Layouts.ThreeCol as ThreeCol
import Berks.Layouts.Vert as Vert
import XMonad.Actions.MouseResize as MR
import XMonad.Hooks.ManageDocks as MD
import XMonad.Layout as L
  ( (|||),
  )
import XMonad.Layout.MultiToggle as MT
  ( EOT (EOT),
    -- , HCons
    -- , MultiToggle
    mkToggle,
    single,
    (??),
  )
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

layoutModifier = avoidStruts . mouseResize . windowArrange . windowNavigation

layout =
  layoutModifier $
    mkToggle (single REFLECTY) $
      mkToggle (single REFLECTX) $
        mkToggle (NBFULL ?? NOBORDERS ?? EOT) layouts'
  where
    layouts' = monocle ||| vert ||| horiz ||| threeCol
