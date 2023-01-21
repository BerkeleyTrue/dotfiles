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
  renamed [Replace " ﰦ "] $
    withBorder 4 $
      spacingRaw True (Border 4 4 0 6) True (Border 0 0 6 0) True $
        limitWindows 7 $
          ThreeColMid 1 (3 / 100) (1 / 2)
