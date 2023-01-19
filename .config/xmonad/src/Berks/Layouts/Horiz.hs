module Berks.Layouts.Horiz
  ( horiz,
  )
where

import Berks.Layouts.Vert
import XMonad
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation

horiz = renamed [Replace " \xf8dd "] $ smartSpacing 1 $ Mirror vert
