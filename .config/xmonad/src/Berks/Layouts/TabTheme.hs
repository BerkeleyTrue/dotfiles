{-# OPTIONS_GHC -Wall #-}

module Berks.Layouts.TabTheme
  ( tabbedDecorator,
  )
where

-- import XMonad.Config.Prime

import Berks.Colors
import Berks.Font
import XMonad.Core
import XMonad.Layout.Decoration hiding
  ( LayoutModifier,
  )
import XMonad.Layout.Tabbed

-- Layouts:
tabTheme :: Theme
tabTheme =
  def
    { fontName = font,
      inactiveTextColor = comment,
      inactiveColor = background,
      inactiveBorderColor = background,
      activeTextColor = foreground,
      activeColor = cyan,
      activeBorderColor = cyan
    }

tabbedDecorator ::
  (Eq a, LayoutClass l a) =>
  l a ->
  ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) l a
tabbedDecorator = addTabs shrinkText tabTheme
