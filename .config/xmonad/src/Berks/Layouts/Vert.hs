module Berks.Layouts.Vert
  ( vert,
  )
where

import XMonad
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation

vert =
  renamed [Replace " \xf8de "] $
    reflectHoriz $
      smartSpacing 1 $
        windowNavigation $
          Tall 1 (3 / 100) (1 / 2)
