module Berks.Layouts.Vert
  ( vert
  ) where

import qualified XMonad as X
import qualified XMonad.Layout as L
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.Renamed as RN
import qualified XMonad.Layout.WindowNavigation as WN

type Vert
   = LM.ModifiedLayout RN.Rename (LM.ModifiedLayout WN.WindowNavigation L.Tall) X.Window

vert :: Vert
vert =
  RN.renamed [RN.Replace "Vert"] $
  WN.windowNavigation $ L.Tall 1 (3 / 100) (1 / 2)
