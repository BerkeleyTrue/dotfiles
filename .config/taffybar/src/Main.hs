module Main
  ( main,
  )
where

import Berks.Bars.Delora (delorasBars)
import Berks.Bars.Rena (renasBars)
import Berks.Bars.Utils (getMonitorCount)
import Control.Monad.Trans.Class (lift)
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
  startTaffybar $
    TaffybarConfig
      { dbusClientParam = Nothing,
        startupHook = return (),
        getBarConfigsParam = do
          monitors <- lift getMonitorCount
          hostName <- lift getHostName
          bars <- lift $ getBars hostName monitors
          lift $ putStrLn $ "Starting Taffybar on " ++ hostName ++ " with " ++ show monitors ++ " monitors"
          return bars,
        cssPaths = [],
        errorMsg = Nothing
      }
