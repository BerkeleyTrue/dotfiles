module Main
  ( main,
  )
where

import Berks.Bars.Delora (delorasBars)
-- import System.Log.Logger
--   ( Priority (..),
--     getLogger,
--     saveGlobalLogger,
--     setLevel,
--   )
import Network.HostName (getHostName)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..),
    TaffybarConfig (..),
  )

getBars :: String -> IO [BarConfig]
getBars = const delorasBars

main :: IO ()
main = do
  hostName <- getHostName
  bars <- getBars hostName
  -- logger <- getLogger "Berks.WidgetUtils"
  -- logger2 <- getLogger "Berks.WidgetUtils"
  -- saveGlobalLogger $ setLevel INFO logger
  -- saveGlobalLogger $ setLevel DEBUG logger2
  putStrLn $ "Starting Taffybar on " ++ hostName
  putStrLn "Starting Taffybar"
  startTaffybar $
    TaffybarConfig
      { dbusClientParam = Nothing,
        startupHook = return (),
        getBarConfigsParam = return bars,
        cssPaths = [],
        errorMsg = Nothing
      }
