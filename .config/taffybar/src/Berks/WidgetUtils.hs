module Berks.WidgetUtils
  ( decorateWithClassname,
    setWidgetClassname,
    setWidgetClassnameFromString,
    runCommandWithDefault,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text as T hiding
  ( filter,
  )
import GI.Gtk
import System.Log.Logger
import System.Taffybar.Util
import System.Taffybar.Widget.Util
import Text.Printf (printf)

setWidgetClassnameFromString :: MonadIO m => String -> Widget -> m Widget
setWidgetClassnameFromString = setWidgetClassname . pack

setWidgetClassname :: MonadIO m => Text -> Widget -> m Widget
setWidgetClassname classname =
  buildContentsBox >=> flip widgetSetClassGI classname

decorateWithClassname :: MonadIO m => Text -> m Widget -> m Widget
decorateWithClassname classname builder =
  builder >>= setWidgetClassname classname

runCommandWithDefault :: FilePath -> [String] -> String -> IO String
runCommandWithDefault cmd args def =
  filter (/= '\n') <$> (runCommand cmd args >>= either logError return)
  where
    logError err =
      logM "Berks.WidgetUtils" ERROR (printf "Got error in CommandRunner %s" err)
        >> return def
