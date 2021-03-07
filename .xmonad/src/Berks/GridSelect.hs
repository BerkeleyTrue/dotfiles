module Berks.GridSelect
  ( createAppGridSpawner
  ) where

import Data.Tuple
import XMonad
import XMonad.Actions.GridSelect

buildConf font =
  def
    { gs_cellheight = 40
    , gs_cellwidth = 200
    , gs_cellpadding = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font = font
    }

selections =
  [("Kitty", "kitty"), ("Firefox", "firefox"), ("PCManFM", "pcmanfm")]

-- spawnWhenJust :: Maybe String -> X ()
spawnWhenJust = flip whenJust spawn

createAppGridSpawner :: String -> X ()
createAppGridSpawner font =
  gridselect (buildConf font) selections >>= spawnWhenJust
