module Berks.Layouts.ThreeCol
  ( threeCol,
  )
where

import XMonad.Layout.LimitWindows as LW
  ( limitWindows,
  )
import XMonad.Layout.Renamed as RN
  ( Rename (Replace),
    renamed,
  )
import XMonad.Layout.Spacing as Spacing
  ( Border (..),
    spacingRaw,
  )
import XMonad.Layout.ThreeColumns as TC
  ( ThreeCol (..),
  )

threeCol =
  renamed [RN.Replace " ﰦ "] $
    spacingRaw True (Border 4 4 0 6) True (Border 0 0 6 0) True $
      limitWindows 7 $
        ThreeColMid 1 (3 / 100) (1 / 2)
