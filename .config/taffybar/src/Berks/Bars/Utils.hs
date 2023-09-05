module Berks.Bars.Utils
  ( createBarStrut,
    createBarConfig,
    createBarConfigs,
    getMonitorCount,
  )
where

import Control.Monad
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Unique
  ( Unique,
    newUnique,
  )
import GI.Gdk.Objects.Display
  ( displayGetNMonitors,
  )
import GI.Gdk.Objects.Screen
  ( screenGetDefault,
    screenGetDisplay,
  )
import Graphics.UI.GIGtkStrut
  ( StrutAlignment (Beginning),
    StrutConfig (..),
    StrutPosition,
    StrutSize
      ( ExactSize,
        ScreenRatio
      ),
  )
import System.Taffybar.Context (BarConfig)
import Prelude

getMonitorCount :: IO Int32
getMonitorCount =
  screenGetDefault
    >>= maybe
      (putStrLn "get default returned null" >> return 0)
      (screenGetDisplay >=> displayGetNMonitors)

createBarStrut :: Int32 -> StrutPosition -> Maybe Int32 -> StrutConfig
createBarStrut monitor pos size =
  StrutConfig
    { strutHeight = ExactSize $ fromMaybe 30 size,
      strutWidth = ScreenRatio 1,
      strutXPadding = 0,
      strutYPadding = 0,
      strutAlignment = Beginning,
      strutPosition = pos,
      strutMonitor = Just monitor,
      strutDisplayName = Nothing
    }

createBarConfig :: (Unique -> BarConfig) -> IO BarConfig
createBarConfig f = f <$> newUnique

createBarConfigs :: [Unique -> BarConfig] -> IO [BarConfig]
createBarConfigs = mapM createBarConfig
