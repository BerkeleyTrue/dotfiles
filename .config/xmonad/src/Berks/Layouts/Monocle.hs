module Berks.Layouts.Monocle
  ( monocle,
    Monocle,
  )
where

import XMonad
import XMonad.Layout.Decoration hiding
  ( LayoutModifier,
  )
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts

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
