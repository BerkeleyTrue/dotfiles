module Berks.Layouts.Monocle
  ( monocle
  , Monocle
  ) where

import qualified XMonad as X
import qualified XMonad.Layout as L
import qualified XMonad.Layout.Decoration as D hiding (LayoutModifier)
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.LimitWindows as LW
import qualified XMonad.Layout.NoBorders as NB
import qualified XMonad.Layout.Renamed as RN
import qualified XMonad.Layout.Simplest as SM
import qualified XMonad.Layout.SubLayouts as SL
import qualified XMonad.Layout.Tabbed as T

import qualified Berks.Layouts.TabTheme as TabTheme

type SimplyLayout = LM.ModifiedLayout NB.SmartBorder SM.Simplest

type LimitedFull = LM.ModifiedLayout LW.LimitWindows L.Full

type Monocle
   = LM.ModifiedLayout RN.Rename (LM.ModifiedLayout (SL.Sublayout SimplyLayout) LimitedFull) X.Window

monocle :: Monocle
monocle =
  RN.renamed [RN.Replace "Monocle"] $
  SL.subLayout [] (NB.smartBorders SM.Simplest) $ LW.limitWindows 20 L.Full
