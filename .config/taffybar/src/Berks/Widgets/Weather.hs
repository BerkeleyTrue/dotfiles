module Berks.Widgets.Weather
  ( weatherWidget,
  )
where

import Berks.WidgetUtils (decorateWithClassname)
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk (Widget)
import Berks.Information.Weather (getWeatherInfoFor, WeatherInfo(..), WindInfo (..))
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import Data.Text (pack, Text)


getLocalWheather :: IO [WeatherInfo]
getLocalWheather =  getWeatherInfoFor "KBDU"

formatWeatherInfo :: [WeatherInfo] -> Text
formatWeatherInfo [] = "N/A"
formatWeatherInfo (_:_:_) = "N/A"
formatWeatherInfo [wi] = pack $ show (tempF wi) <> "° \57982 " <> windMph (windInfo wi) <> "mph"

weatherWidget :: MonadIO m => m Widget
weatherWidget = decorateWithClassname "weather" $ pollingLabelNew 10 $ formatWeatherInfo <$> getLocalWheather
