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
-- import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

type ToggleReflectX = MultiToggle (HCons REFLECTX EOT)

type ToggleReflectY = MultiToggle (HCons REFLECTY EOT)

type ToggleFull = MultiToggle (HCons StdTransformers EOT)

type ToggleNoBorders = MultiToggle (HCons StdTransformers EOT)

toggleReflectX :: (LayoutClass l a) => l a -> ToggleReflectX l a
toggleReflectX = mkToggle $ single REFLECTX

toggleReflectY :: (LayoutClass l a) => l a -> ToggleReflectY l a
toggleReflectY = mkToggle $ single REFLECTY

toggleFull :: (LayoutClass l a) => l a -> ToggleFull l a
toggleFull = mkToggle $ single FULL

toggleNoBorders :: (LayoutClass l a) => l a -> ToggleNoBorders l a
toggleNoBorders = mkToggle $ single NOBORDERS

-- type MouseResizeModifier = ModifiedLayout MouseResize
--
-- type WindowArrangerModifier = ModifiedLayout WindowArranger
--
-- type WindowNavigationModifier = ModifiedLayout WindowNavigation
--
-- type AvoidStrutsModifier = ModifiedLayout AvoidStruts

-- type MyLayoutModifiers l =
--     ( MouseResizeModifier
--         ( WindowArrangerModifier
--             (WindowNavigationModifier (AvoidStrutsModifier l))
--         )
--     )

-- layoutModifier :: (LayoutClass l a) => l a -> MyLayoutModifiers l a -- ScreenCornersLayoutHook is not exported!
layoutModifier =
  screenCornerLayoutHook
    . mouseResize
    . windowArrange
    . windowNavigation
    . avoidStruts

type ChooseLayouts = Choose ThreeColLayout Monocle

chooseLayout :: ChooseLayouts Window
chooseLayout = threeCol ||| monocle

-- type MyLayout =
--   MyLayoutModifiers
--     ( ToggleReflectY
--         (ToggleReflectX (ToggleNoBorders (ToggleFull ChooseLayouts)))
--     )

-- layout :: MyLayout Window
layout =
  layoutModifier $
    toggleReflectY $
      toggleReflectX $
        toggleNoBorders $
          toggleFull chooseLayout
