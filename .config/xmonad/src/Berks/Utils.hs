module Berks.Utils
  ( center
  , centerWindow
  ) where

import qualified XMonad.StackSet as W

center :: Rational -> Rational
center ratio = (1 - ratio) / 2

centerWindow = W.RationalRect x y w h
  where
    w = 2 / 5
    h = 3 / 5
    x = center w
    y = center h
