module Berks.Widgets.Divider
  ( dividerWidget,
    plainDividerWidget,
  )
where

import Berks.Colors
import Control.Monad.IO.Class (MonadIO)
import Data.Text (pack)
import GI.Gtk
import System.Taffybar.Widget (vFillCenter)

dividerWidget :: (MonadIO m) => Maybe Hex -> m Widget
dividerWidget maybeColor = do
  color <- case maybeColor of
    Just color -> return color
    Nothing -> return (subtext1 hexes)
  grid <- gridNew
  label <- labelNew Nothing

  _ <- onWidgetRealize label $ do
    labelSetMarkup label $ pack $ "<span fgcolor='" ++ color ++ "'> | </span>"

  vFillCenter label
  vFillCenter grid
  containerAdd grid label
  widgetShowAll grid
  toWidget grid

plainDividerWidget :: (MonadIO m) => m Widget
plainDividerWidget = dividerWidget Nothing
