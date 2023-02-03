module Berks.Widgets.Divider
  ( dividerWidget,
  )
where

import GI.Gtk
import System.Taffybar.Widget (vFillCenter)
import System.Taffybar.Context (TaffyIO)

dividerWidget :: TaffyIO Widget
dividerWidget = do
  grid <- gridNew
  label <- labelNew (Just " | ")
  vFillCenter label
  vFillCenter grid
  containerAdd grid label
  widgetShowAll grid
  toWidget grid
