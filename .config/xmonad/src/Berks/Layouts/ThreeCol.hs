module Berks.Layouts.ThreeCol
  ( threeCol,
  )
where

import XMonad.Layout.LimitWindows
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

threeCol =
  renamed [Replace " ﰦ "] $
    spacingRaw True (Border 4 4 0 6) True (Border 0 0 6 0) True $
      limitWindows 7 $
        ThreeColMid 1 (3 / 100) (1 / 2)
