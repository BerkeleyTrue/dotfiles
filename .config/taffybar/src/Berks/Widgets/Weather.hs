module Berks.Widgets.Weather
  ( weatherWidget,
  )
where

import Berks.Colors
import Berks.Information.Weather
  ( WeatherInfo (..),
    WindInfo (..),
    getWeatherInfoFor,
  )
import Berks.WidgetUtils (decorateWithClassname)
import Control.Monad.IO.Class (MonadIO)
import Data.Text
  ( Text,
    pack,
  )
import GI.Gtk (Widget)
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )
import System.Taffybar.Widget.Util (colorize)

getLocalWheather :: IO [WeatherInfo]
getLocalWheather = getWeatherInfoFor "KBDU"

formatWeatherInfo :: [WeatherInfo] -> Text
formatWeatherInfo [] = "N/A"
formatWeatherInfo (_ : _ : _) = "N/A"
formatWeatherInfo [wi] =
  pack $
    colorize pinkHex "" (show $ tempF wi)
      <> colorize "white" "" "°"
      <> colorize greenHex "" (" \57982 " <> windMph (windInfo wi) <> "mph")

weatherWidget :: MonadIO m => m Widget
weatherWidget =
  decorateWithClassname "weather" $
    pollingLabelNew 10 $
      formatWeatherInfo
        <$> getLocalWheather
