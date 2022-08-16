module Berks.Layouts.Horiz
  ( horiz
  ) where

import qualified XMonad as X
import qualified XMonad.Layout as L
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.Renamed as RN
import qualified XMonad.Layout.Spacing as Spacing
import qualified XMonad.Layout.WindowNavigation as WN

import qualified Berks.Layouts.Vert as Vert

horiz =
  RN.renamed [RN.Replace "="] $ Spacing.smartSpacing 1 $ L.Mirror Vert.vert
