module Berks.WidgetUtils
  ( decorateWithClassname,
    setWidgetClassname,
    setWidgetClassnameFromString,
    runCommandWithDefault,
    updateButtonLabelCallback,
    pollingLabelButtonNewWithVariableDelay,
    pollingLabelButtonNew,
    buttonWithClickHandler,
  )
where

import Control.Concurrent (killThread)
import Control.Exception.Enclosed as E
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

updateButtonLabelCallback :: MonadIO m => IO Text -> Button -> m ()
updateButtonLabelCallback action obj = do
  label <- liftIO action
  setButtonLabel obj label

pollingLabelButtonNewWithVariableDelay ::
  (MonadIO m) =>
  -- | Text to display
  IO (String, Double) ->
  -- | handleClick
  (Button -> IO ()) ->
  m Widget
pollingLabelButtonNewWithVariableDelay action handleClick = do
  button <- buttonNew
  label <- labelNew Nothing
  buttonSetImage button $ Just label
  _ <- onButtonClicked button $ handleClick button

  let updateLabel (labelStr, delay) = do
        postGUIASync $ labelSetMarkup label $ pack labelStr
        logM "Berks.WidgetUtils" DEBUG $
          printf "Polling label Button delay was %s" $
            show delay
        return delay

      updateLabelHandlingErrors =
        E.tryAny action >>= either (const $ return 1) updateLabel

  _ <- onWidgetRealize button $ do
    sampleThread <- foreverWithVariableDelay updateLabelHandlingErrors
    void $ onWidgetUnrealize button $ killThread sampleThread

  widgetShowAll button
  toWidget button

pollingLabelButtonNew ::
  (MonadIO m) =>
  -- | Text to display
  IO String ->
  -- | label
  (Button -> IO ()) ->
  -- | handleClick
  m Widget
pollingLabelButtonNew action =
  pollingLabelButtonNewWithVariableDelay (fmap (,1) action)

buttonWithClickHandler ::
  (MonadIO m) => String -> (Button -> IO ()) -> m Widget
buttonWithClickHandler labelStr handler = do
  label <- labelNew Nothing
  labelSetMarkup label (pack labelStr)
  button <- buttonNew
  _ <- onButtonClicked button $ handler button
  _ <- buttonSetImage button $ Just label

  widgetShowAll button
  toWidget button
