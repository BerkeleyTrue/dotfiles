module Berks.Widgets.Layout
  ( layoutWidget,
  )
where

import Berks.Colors (hexes, red)
import Data.Text
  ( pack,
    unpack,
  )
import GI.Gtk (Widget)
-- import GI.Glib (markupEscapeText)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Layout
  ( LayoutConfig (formatLayout),
    defaultLayoutConfig,
    layoutNew,
  )
import System.Taffybar.Widget.Util (colorize)

layoutWidget :: TaffyIO Widget
layoutWidget = layoutNew defaultLayoutConfig {formatLayout = formatLayout'}
  where
    formatLayout' =
      return . pack . colorize (red hexes) "" . ("[" <>) . (<> "]") . unpack
