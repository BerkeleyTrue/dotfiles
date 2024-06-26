module Berks.Layouts.ThreeCol
  ( threeCol,
    ThreeColLayout,
  )
where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

type ThreeColLayout =
  ModifiedLayout
    Rename
    ( ModifiedLayout
        WithBorder
        (ModifiedLayout Spacing (ModifiedLayout LimitWindows ThreeCol))
    )

threeCol :: ThreeColLayout Window
threeCol =
  renamed [Replace " \xf056d "] $
    withBorder 4 $
      spacingRaw False (Border 8 18 0 12) True (Border 0 0 12 0) True $
        limitWindows 7 $
          ThreeColMid 1 (3 / 100) (1 / 2)
