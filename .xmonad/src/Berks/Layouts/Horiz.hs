module Berks.Layouts.Horiz
  ( horiz
  ) where

import qualified XMonad as X
import qualified XMonad.Layout as L
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.Renamed as RN
import qualified XMonad.Layout.WindowNavigation as WN

import qualified Berks.Layouts.Vert as Vert

type Horiz
   = LM.ModifiedLayout RN.Rename (L.Mirror (LM.ModifiedLayout RN.Rename (LM.ModifiedLayout WN.WindowNavigation L.Tall))) X.Window

horiz :: Horiz
horiz = RN.renamed [RN.Replace "Horiz"] $ L.Mirror Vert.vert
