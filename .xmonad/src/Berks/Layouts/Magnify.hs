module Berks.Layouts.Magnify
  ( magnify
  ) where

import qualified XMonad as X

-- layouts
import qualified XMonad.Layout as L
import qualified XMonad.Layout.ResizableTile as RT
import qualified XMonad.Layout.Simplest as SM

import qualified XMonad.Layout.Decoration as D hiding (LayoutModifier)

-- modifiers
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.LimitWindows as LW
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.Layout.NoBorders as NB
import qualified XMonad.Layout.Renamed as RN
import qualified XMonad.Layout.SubLayouts as SL
import qualified XMonad.Layout.Tabbed as T
import qualified XMonad.Layout.WindowNavigation as WN

import qualified Berks.Layouts.TabTheme as TabTheme

type SimplyLayout = LM.ModifiedLayout NB.SmartBorder SM.Simplest

type LimitedSpaced = LM.ModifiedLayout LW.LimitWindows RT.ResizableTall

type LimitedFull = LM.ModifiedLayout LW.LimitWindows L.Full

type MagSpaced = LM.ModifiedLayout Mag.Magnifier LimitedSpaced

type TabbedShrink = D.Decoration T.TabbedDecoration D.DefaultShrinker

type Magnify
   = D.ModifiedLayout RN.Rename (D.ModifiedLayout WN.WindowNavigation (D.ModifiedLayout TabbedShrink (D.ModifiedLayout (SL.Sublayout SimplyLayout) MagSpaced))) X.Window

magnify :: Magnify
magnify =
  RN.renamed [RN.Replace "Magnify"] $
  WN.windowNavigation $
  TabTheme.tabbedDecorator $
  SL.subLayout [] (NB.smartBorders SM.Simplest) $
  Mag.magnifier $ LW.limitWindows 12 $ RT.ResizableTall 1 (3 / 100) (1 / 2) []
