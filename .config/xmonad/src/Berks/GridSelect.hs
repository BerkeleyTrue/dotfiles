module Berks.GridSelect
  ( createAppGridSpawner,
  )
where

import Berks.Font
import XMonad
import XMonad.Actions.GridSelect

gsConf :: GSConfig String
gsConf =
  def
    { gs_cellheight = 40,
      gs_cellwidth = 200,
      gs_cellpadding = 6,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_font = font
    }

selections :: [(String, String)]
selections =
  [ ("Firefox", "org.mozilla.firefox"),
    ("Frame", "frame"),
    ("PCManFM", "pcmanfm"),
    ("Discord", "com.discordapp.Discord"),
    ("Slack", "com.slack.Slack")
  ]

spawnWhenJust :: Maybe String -> X ()
spawnWhenJust = flip whenJust spawn

createAppGridSpawner :: () -> X ()
createAppGridSpawner _ = gridselect gsConf selections >>= spawnWhenJust
