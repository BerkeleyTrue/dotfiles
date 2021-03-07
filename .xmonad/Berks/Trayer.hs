module Berks.Trayer
  ( spawnTray
  ) where

import Data.List
import XMonad hiding (kill)
import XMonad.Util.Run (safeSpawn)

kill :: MonadIO m => FilePath -> m ()
kill prog = safeSpawn "killall" [prog]

trayer = "trayer" :: FilePath

flatMap x = x >>= \x -> [head x, last x]

spawnTray :: MonadIO m => m ()
spawnTray = do
  kill "trayer"
  safeSpawn trayer $
    flatMap
      [ ["--edge", "top"]
      , ["--align", "right"]
      , ["--expand", "true"]
      , ["--widthtype", "request"]
      , ["--height", "18"]
      , ["--padding", "6"]
      , ["--monitor", "0"]
      , ["--alpha", "0"]
      , ["--transparent", "true"]
      , ["--tint", "0x282c34"]
      , ["--SetPartialStrut", "false"]
      , ["--SetDockType", "true"]
      ]
