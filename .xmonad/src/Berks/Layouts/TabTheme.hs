{-# OPTIONS_GHC -Wall #-}

module Berks.Layouts.TabTheme
  ( tabbedDecorator
  ) where

-- import qualified XMonad.Config.Prime as Prime
import qualified XMonad.Core as Core
import qualified XMonad.Layout.Decoration as D hiding (LayoutModifier)
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.Tabbed as T

import Berks.Colors as Cl
import Berks.Font as Fs

-- Layouts:
tabTheme :: T.Theme
tabTheme =
  T.def
    { T.fontName = Fs.font
    , T.inactiveTextColor = Cl.comment
    , T.inactiveColor = Cl.background
    , T.inactiveBorderColor = Cl.background
    , T.activeTextColor = Cl.foreground
    , T.activeColor = Cl.cyan
    , T.activeBorderColor = Cl.cyan
    }

tabbedDecorator ::
     (Eq a, Core.LayoutClass l a)
  => l a
  -> LM.ModifiedLayout (D.Decoration T.TabbedDecoration D.DefaultShrinker) l a
tabbedDecorator = T.addTabs D.shrinkText tabTheme
