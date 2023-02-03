module Berks.Layouts.Horiz
  ( horiz,
  )
where

import Berks.Layouts.Vert
import XMonad
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

horiz = renamed [Replace " \xf8dd "] $ smartSpacing 1 $ Mirror vert
