module Berks.Layouts.ThreeCol
  ( threeCol,
  )
where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

type LimitedThreeCol = ModifiedLayout LimitWindows ThreeCol

type SpacedThreeCol = ModifiedLayout Spacing LimitedThreeCol

type ThreeColLayout = ModifiedLayout Rename SpacedThreeCol

threeCol :: ThreeColLayout Window
threeCol =
  renamed [Replace " ﰦ "] $
    spacingRaw True (Border 4 4 0 6) True (Border 0 0 6 0) True $
      limitWindows 7 $
        ThreeColMid 1 (3 / 100) (1 / 2)
