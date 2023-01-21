module Berks.Layouts.Monocle
  ( monocle,
    Monocle,
  )
where

import Berks.Layouts.TabTheme
import XMonad
import XMonad.Layout
import XMonad.Layout.Decoration hiding
  ( LayoutModifier,
  )
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed

type SimplyLayout = ModifiedLayout SmartBorder Simplest

type LimitedFull = ModifiedLayout LimitWindows Full

type Monocle =
  ModifiedLayout
    Rename
    (ModifiedLayout (Sublayout SimplyLayout) LimitedFull)

monocle :: Monocle Window
monocle =
  renamed [Replace " ﱢ "] $
    subLayout [] (smartBorders Simplest) $
      limitWindows
        20
        Full
