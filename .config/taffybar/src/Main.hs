module Main
  ( main,
  )
where

import Berks.Bars.Delora (delorasBars)
import Berks.Bars.Rena (renasBars)
import Berks.Bars.Utils (getMonitorCount)
import Data.Int (Int32)
import Network.HostName (getHostName)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..),
    TaffybarConfig (..),
  )

getBars :: String -> Int32 -> IO [BarConfig]
getBars hostName monitors = do
  case hostName of
    "rena" -> renasBars
    _ -> delorasBars monitors

main :: IO ()
main = do
  monitors <- getMonitorCount
  hostName <- getHostName
  bars <- getBars hostName monitors
  putStrLn $ "Starting Taffybar on " ++ hostName ++ " with " ++ show monitors ++ " monitors"
  startTaffybar $
    TaffybarConfig
      { dbusClientParam = Nothing,
        startupHook = return (),
        getBarConfigsParam = return bars,
        cssPaths = [],
        errorMsg = Nothing
      }
