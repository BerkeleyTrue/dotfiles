{-# OPTIONS_GHC -Wall #-}

module Berks.Layouts.Main
  ( layout
  ) where

import qualified Berks.Layouts.Horiz as Horiz
import qualified Berks.Layouts.Magnify as Mag
import qualified Berks.Layouts.Monocle as Mono
import qualified Berks.Layouts.Vert as Vert
import qualified XMonad.Actions.MouseResize as MR
import qualified XMonad.Hooks.ManageDocks as MD
import qualified XMonad.Layout as L
import qualified XMonad.Layout.MultiToggle as MT
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

layout =
  MD.avoidStruts . MR.mouseResize . WA.windowArrange $
  MT.mkToggle (MT.single LR.REFLECTY) $
  MT.mkToggle (MT.single LR.REFLECTX) $
  MT.mkToggle (MT.NBFULL MT.?? MT.NOBORDERS MT.?? MT.EOT) layouts'
  where
    layouts' = Mag.magnify L.||| Vert.vert L.||| Mono.monocle L.||| Horiz.horiz
