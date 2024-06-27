module Berks.Widgets.Weather
  ( weatherWidget,
  )
where

import Berks.Colors
import Berks.WidgetUtils (decorateWithClassname)
import Control.Exception qualified as CE
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8 qualified as BChar
import Data.ByteString.Lazy (toStrict)
import Data.Text
  ( Text,
    pack,
  )
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk (Widget)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Conduit
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )
import System.Taffybar.Widget.Util (colorize)

baseUrl :: String
baseUrl = "https://wttr.in/"

mkUrl :: String -> String
mkUrl zipCode = baseUrl ++ zipCode

getWeather :: String -> String -> IO Text
getWeather zipCode format =
  CE.catch
    ( do
        request <- setRequestHeader . setQueryString [("format", Just $ BChar.pack format)] <$> parseUrlThrow (mkUrl zipCode)
        man <- getGlobalManager
        res <- httpLbs request man
        return . decodeUtf8 . toStrict $ responseBody res
    )
    errHandler
  where
    errHandler :: CE.SomeException -> IO Text
    errHandler err = return $ pack ("<Weather: Could not retrieve data> " ++ show err ++ " " ++ format)

    setRequestHeader :: Request -> Request
    setRequestHeader req = req {requestHeaders = [("User-Agent", "curl")]}

getLocalWeather :: IO Text
getLocalWeather = getWeather "94565" ("%C " <> colorize (pink hexes) "" "%t" <> colorize (green hexes) "" " %w")

weatherWidget :: (MonadIO m) => m Widget
weatherWidget =
  decorateWithClassname "weather" $
    pollingLabelNew 20 getLocalWeather
