module Berks.Layouts.ThreeCol
  ( threeCol
  ) where

import qualified XMonad as X
import qualified XMonad.Layout as L
import XMonad.Layout.LimitWindows as LW (limitWindows)
import qualified XMonad.Layout.NoBorders as NB
import qualified XMonad.Layout.Renamed as RN
import qualified XMonad.Layout.Simplest as SM
import XMonad.Layout.ThreeColumns as TC (ThreeCol(..))
import qualified XMonad.Layout.WindowNavigation as WN

threeCol =
  RN.renamed [RN.Replace " ﰦ "] $
  limitWindows 7 $ ThreeColMid 1 (3 / 100) (1 / 2)
