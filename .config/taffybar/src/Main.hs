module Main
  ( main,
  )
where

import Berks.Bars.Delora (delorasBars)
import Berks.Bars.Rena (renasBars)
import Network.HostName (getHostName)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..),
    TaffybarConfig (..),
  )

getBars :: String -> IO [BarConfig]
getBars hostName = do
  case hostName of
    "rena" -> renasBars
    _ -> delorasBars

main :: IO ()
main = do
  hostName <- getHostName
  bars <- getBars hostName
  putStrLn $ "Starting Taffybar on " ++ hostName
  startTaffybar $
    TaffybarConfig
      { dbusClientParam = Nothing,
        startupHook = return (),
        getBarConfigsParam = return bars,
        cssPaths = [],
        errorMsg = Nothing
      }
