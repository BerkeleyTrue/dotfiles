module Berks.Bars.Utils
  ( createBarStrut,
    createBarConfig,
    createBarConfigs,
  )
where

import Data.Int (Int32)
import Data.Maybe (fromMaybe)
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
