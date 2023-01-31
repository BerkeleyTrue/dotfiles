module Berks.WidgetUtils
  ( decorateWithClassname,
    setWidgetClassname,
    setWidgetClassnameFromString
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import GI.Gtk
import System.Taffybar.Widget.Util

setWidgetClassnameFromString :: MonadIO m => String -> Widget -> m Widget
setWidgetClassnameFromString = setWidgetClassname . pack

setWidgetClassname :: MonadIO m => Text -> Widget -> m Widget
setWidgetClassname classname =
  buildContentsBox >=> flip widgetSetClassGI classname

decorateWithClassname :: MonadIO m => Text -> m Widget -> m Widget
decorateWithClassname classname builder =
  builder >>= setWidgetClassname classname
