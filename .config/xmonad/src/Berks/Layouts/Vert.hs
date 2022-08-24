module Berks.Layouts.Vert
  ( vert
  ) where

import qualified XMonad as X
import qualified XMonad.Layout as L
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.Renamed as RN
import qualified XMonad.Layout.Spacing as Spacing
import qualified XMonad.Layout.WindowNavigation as WN

vert =
  RN.renamed [RN.Replace " \xf8de "] $
  Spacing.smartSpacing 1 $ WN.windowNavigation $ L.Tall 1 (3 / 100) (1 / 2)
