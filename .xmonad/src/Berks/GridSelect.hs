module Berks.GridSelect
  ( createAppGridSpawner
  ) where

import qualified Berks.Font as Fs
import Data.Tuple
import XMonad
import XMonad.Actions.GridSelect

gsConf =
  def
    { gs_cellheight = 40
    , gs_cellwidth = 200
    , gs_cellpadding = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font = Fs.font
    }

selections =
  [("Kitty", "kitty"), ("Firefox", "firefox"), ("PCManFM", "pcmanfm")]

-- spawnWhenJust :: Maybe String -> X ()
spawnWhenJust = flip whenJust spawn

createAppGridSpawner :: () -> X ()
createAppGridSpawner _ = gridselect gsConf selections >>= spawnWhenJust
