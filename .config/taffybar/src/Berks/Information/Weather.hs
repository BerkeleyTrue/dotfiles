-- sourced from https://codeberg.org/xmobar/xmobar/src/branch/master/src/Xmobar/Plugins/Monitors/Weather.hs
module Berks.Information.Weather
  ( WeatherInfo (..),
    WindInfo (..),
    getWeatherInfoFor,
  )
where

import Control.Exception qualified as CE
import Data.ByteString.Lazy.Char8 qualified as B
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Conduit
import System.Log.Logger (errorM)
import Text.ParserCombinators.Parsec

data WindInfo = WindInfo
  { windCardinal :: String,
    -- cardinal direction
    windAzimuth :: String,
    -- azimuth direction
    windMph :: String,
    -- speed (MPH)
    windKnots :: String,
    -- speed (knot)
    windKmh :: String,
    -- speed (km/h)
    windMs :: String -- speed (m/s)
  }
  deriving (Show)

data WeatherInfo = WI
  { stationPlace :: String,
    stationState :: String,
    year :: String,
    month :: String,
    day :: String,
    hour :: String,
    windInfo :: WindInfo,
    visibility :: String,
    skyCondition :: String,
    weather :: String,
    tempC :: Int,
    tempF :: Int,
    dewPointC :: Int,
    dewPointF :: Int,
    humidity :: Int,
    pressure :: Int
  }
  deriving (Show)

defUrl :: String
defUrl = "https://tgftp.nws.noaa.gov/data/observations/metar/decoded/"

getNumbersAsString :: Parser String
getNumbersAsString = skipMany space >> many1 digit >>= \n -> return n

skipRestOfLine :: Parser Char
skipRestOfLine = do
  _ <- many $ noneOf "\n\r"
  newline

getAllBut :: String -> Parser String
getAllBut s = manyTill (noneOf s) (char $ head s)

getAfterString :: String -> Parser String
getAfterString s =
  do
    _ <- try $ manyTill skipRestOfLine $ string s
    manyTill anyChar newline
    <|> return ""

skipTillString :: String -> Parser String
skipTillString s = manyTill skipRestOfLine $ string s

parseTime :: Parser (String, String, String, String)
parseTime = do
  y <- getNumbersAsString
  _ <- char '.'
  m <- getNumbersAsString
  _ <- char '.'
  d <- getNumbersAsString
  _ <- char ' '
  (h : hh : mi : mimi) <- getNumbersAsString
  _ <- char ' '
  return (y, m, d, h : hh : ":" ++ mi : mimi)

emptyWindInfo :: WindInfo
emptyWindInfo = WindInfo "μ" "μ" "0" "0" "0" "0"

parseWind :: Parser WindInfo
parseWind =
  let tospace = manyTill anyChar (char ' ')
      toKmh knots = knots $* 1.852
      toMs knots = knots $* 0.514
      ($*) :: String -> Double -> String
      op1 $* op2 = show (round ((read op1 :: Double) * op2) :: Integer)

      -- Occasionally there is no wind and a METAR report gives simply, "Wind: Calm:0"
      wind0 = do
        _ <- manyTill skipRestOfLine (string "Wind: Calm:0")
        return emptyWindInfo
      windVar = do
        _ <- manyTill skipRestOfLine (string "Wind: Variable at ")
        mph <- tospace
        _ <- string "MPH ("
        knot <- tospace
        _ <- manyTill anyChar newline
        return $ WindInfo "μ" "μ" mph knot (toKmh knot) (toMs knot)
      wind = do
        _ <- manyTill skipRestOfLine (string "Wind: from the ")
        cardinal <- tospace
        _ <- char '('
        azimuth <- tospace
        _ <- string "degrees) at "
        mph <- tospace
        _ <- string "MPH ("
        knot <- tospace
        _ <- manyTill anyChar newline
        return $ WindInfo cardinal azimuth mph knot (toKmh knot) (toMs knot)
   in try wind0 <|> try windVar <|> try wind <|> return emptyWindInfo

parseTemp :: Parser (Int, Int)
parseTemp = do
  let num = digit <|> char '-' <|> char '.'
  f <- manyTill num $ char ' '
  _ <- manyTill anyChar $ char '('
  c <- manyTill num $ char ' '
  _ <- skipRestOfLine
  return (floor (read c :: Double), floor (read f :: Double))

parseRelativeHumidity :: Parser Int
parseRelativeHumidity = do
  s <- manyTill digit (char '%' <|> char '.')
  return $ read s

parsePressure :: Parser Int
parsePressure = do
  _ <- manyTill anyChar $ char '('
  s <- manyTill digit $ char ' '
  _ <- skipRestOfLine
  return $ read s

{-
    example of 'http://weather.noaa.gov/pub/data/observations/metar/decoded/VTUD.TXT':
        Station name not available
        Aug 11, 2013 - 10:00 AM EDT / 2013.08.11 1400 UTC
        Wind: from the N (350 degrees) at 1 MPH (1 KT):0
        Visibility: 4 mile(s):0
        Sky conditions: mostly clear
        Temperature: 77 F (25 C)
        Dew Point: 73 F (23 C)
        Relative Humidity: 88%
        Pressure (altimeter): 29.77 in. Hg (1008 hPa)
        ob: VTUD 111400Z 35001KT 8000 FEW030 25/23 Q1008 A2977 INFO R RWY30
        cycle: 14
-}
parseWeatherInfosFromData :: Parser [WeatherInfo]
parseWeatherInfosFromData = do
  (stationPlace, stationState) <-
    try (string "Station name not available" >> return ("??", "??"))
      <|> ( do
              stationPlace <- getAllBut ","
              _ <- space
              stationState <- getAllBut "("
              return (stationPlace, stationState)
          )
  _ <- skipRestOfLine >> getAllBut "/"
  (year, month, day, hour) <- parseTime
  wind <- parseWind
  visibility <- getAfterString "Visibility: "
  skyCondition <- getAfterString "Sky conditions: "
  weather <- getAfterString "Weather: "
  _ <- skipTillString "Temperature: "
  (tempC, tempF) <- parseTemp
  _ <- skipTillString "Dew Point: "
  (dewPointC, dewPointF) <- parseTemp
  _ <- skipTillString "Relative Humidity: "
  humidity <- parseRelativeHumidity
  _ <- skipTillString "Pressure (altimeter): "
  pressure <- parsePressure
  _ <- manyTill skipRestOfLine eof
  return
    [ WI
        stationPlace
        stationState
        year
        month
        day
        hour
        wind
        visibility
        skyCondition
        weather
        tempC
        tempF
        dewPointC
        dewPointF
        humidity
        pressure
    ]

stationUrl :: String -> String
stationUrl station = defUrl ++ station ++ ".TXT"

-- | Get the decoded weather data from the given station.
getData :: String -> IO String
getData station =
  CE.catch
    ( do
        request <- parseUrlThrow $ stationUrl station
        man <- getGlobalManager
        res <- httpLbs request man
        return $ B.unpack $ responseBody res
    )
    errHandler
  where
    errHandler :: CE.SomeException -> IO String
    errHandler _ = return "<Could not retrieve data>"

-- | Get WeatherInfo from the given station.
getWeatherInfoFor :: String -> IO [WeatherInfo]
getWeatherInfoFor station = do
  responseData <- getData station
  case parse parseWeatherInfosFromData "" responseData of
    Left err -> do
      errorM "Berks.Information.Weather" $ "Error in weather: " <> show err
      return []
    Right res -> return res
