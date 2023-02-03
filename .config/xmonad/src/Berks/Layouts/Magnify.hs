module Berks.Layouts.Magnify
  ( magnify
  , Magnify
  ) where

import qualified XMonad as X

-- layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Decoration hiding (LayoutModifier)

-- modifiers
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier hiding (magnify)
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import Berks.Layouts.TabTheme as TabTheme

type SimplyLayout = ModifiedLayout SmartBorder Simplest

type LimitedSpaced = ModifiedLayout LimitWindows ResizableTall

type MagSpaced = ModifiedLayout Magnifier LimitedSpaced

type TabbedShrink = Decoration TabbedDecoration DefaultShrinker

type Magnify
   = ModifiedLayout Rename (ModifiedLayout WindowNavigation (ModifiedLayout TabbedShrink (ModifiedLayout (Sublayout SimplyLayout) MagSpaced))) X.Window

magnify :: Magnify
magnify =
  renamed [Replace "Magnify"] $
  windowNavigation $
  TabTheme.tabbedDecorator $
  subLayout [] (smartBorders Simplest) $
  magnifier $ limitWindows 12 $ ResizableTall 1 (3 / 100) (1 / 2) []
