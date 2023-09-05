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
import XMonad.Layout.Spacing

type LimitedFull = ModifiedLayout LimitWindows Full

type Monocle =
  ModifiedLayout
    Rename
    ( ModifiedLayout
        WithBorder
        (ModifiedLayout Spacing LimitedFull)
    )

monocle :: Monocle Window
monocle =
  renamed [Replace " \xf0c8 "] $
    withBorder 4 $
      spacingRaw False (Border 8 18 12 12) True (Border 0 0 0 0) True $
        limitWindows
          20
          Full
