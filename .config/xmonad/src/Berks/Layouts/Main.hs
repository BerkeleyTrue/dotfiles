{-# OPTIONS_GHC -Wall #-}

module Berks.Layouts.Main
  ( layout,
  )
where

import Berks.Layouts.Horiz
import Berks.Layouts.Monocle
import Berks.Layouts.ThreeCol
import Berks.Layouts.Vert
import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
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
