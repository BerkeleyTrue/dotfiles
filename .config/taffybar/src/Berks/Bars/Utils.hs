module Berks.Bars.Utils
  ( createBarStrut,
    createBarConfig,
    createBarConfigs,
  )
where

import Data.Int (Int32)
import Data.Unique
  ( Unique,
    newUnique,
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

createBarStrut :: Int32 -> StrutPosition -> StrutConfig
createBarStrut monitor pos =
  StrutConfig
    { strutHeight = ExactSize 30,
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
