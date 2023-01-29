module Berks.WidgetUtils
  ( deocrateWithClassname,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import GI.Gtk
import System.Taffybar.Widget.Util

setWidgetClassname :: MonadIO m => Text -> Widget -> m Widget
setWidgetClassname classname =
  buildContentsBox >=> flip widgetSetClassGI classname

deocrateWithClassname :: MonadIO m => Data.Text.Text -> m Widget -> m Widget
deocrateWithClassname classname builder =
  builder >>= setWidgetClassname classname
