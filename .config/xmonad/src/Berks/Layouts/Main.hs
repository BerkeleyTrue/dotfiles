{-# OPTIONS_GHC -Wall #-}

module Berks.Layouts.Main
  ( layout
  ) where

import qualified Berks.Layouts.Horiz as Horiz
import qualified Berks.Layouts.Monocle as Mono
import qualified Berks.Layouts.ThreeCol as ThreeCol
import qualified Berks.Layouts.Vert as Vert
import qualified XMonad.Actions.MouseResize as MR
import qualified XMonad.Hooks.ManageDocks as MD
import XMonad.Layout as L ((|||))
import XMonad.Layout.MultiToggle as MT
  ( EOT(EOT)
  -- , HCons
  -- , MultiToggle
  , (??)
  , mkToggle
  , single
  )
import qualified XMonad.Layout.MultiToggle.Instances as MT
  ( StdTransformers(NBFULL, NOBORDERS)
  )
import qualified XMonad.Layout.Reflect as LR
import qualified XMonad.Layout.WindowArranger as WA
import qualified XMonad.Layout.WindowNavigation as WN

layoutModifier =
  MD.avoidStruts . MR.mouseResize . WA.windowArrange . WN.windowNavigation

layout =
  layoutModifier $
  mkToggle (single LR.REFLECTY) $
  mkToggle (single LR.REFLECTX) $
  mkToggle (MT.NBFULL ?? MT.NOBORDERS ?? MT.EOT) layouts'
  where
    layouts' = Mono.monocle ||| Vert.vert ||| Horiz.horiz ||| ThreeCol.threeCol
