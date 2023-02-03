module Berks.Colors
  ( transparent,
    background,
    backgroundHex,
    selection,
    selectionHex,
    foreground,
    foregroundHex,
    comment,
    commentHex,
    cyan,
    cyanHex,
    green,
    greenHex,
    orange,
    orangeHex,
    pink,
    pinkHex,
    purple,
    purpleHex,
    red,
    redHex,
    yellow,
    yellowHex,
    Hex,
  )
where

import Berks.Hexes

type Hex = String

transparent :: RGBA
transparent = (0, 0, 0, 0)

backgroundHex :: Hex
backgroundHex = "#282a36"

background :: RGBA
background = hexToRGBA backgroundHex

selectionHex :: Hex
selectionHex = "#44475a"

selection :: RGBA
selection = hexToRGBA selectionHex

foregroundHex :: Hex
foregroundHex = "#f8f8f2"

foreground :: RGBA
foreground = hexToRGBA foregroundHex

commentHex :: Hex
commentHex = "#6272a4"

comment :: RGBA
comment = hexToRGBA commentHex

cyanHex :: Hex
cyanHex = "#8be9fd"

cyan :: RGBA
cyan = hexToRGBA cyanHex

greenHex :: Hex
greenHex = "#50fa7b"

green :: RGBA
green = hexToRGBA greenHex

orangeHex :: Hex
orangeHex = "#ffb86c"

orange :: RGBA
orange = hexToRGBA orangeHex

pinkHex :: Hex
pinkHex = "#ff79c6"

pink :: RGBA
pink = hexToRGBA pinkHex

purpleHex :: Hex
purpleHex = "#bd93f9"

purple :: RGBA
purple = hexToRGBA purpleHex

redHex :: Hex
redHex = "#ff5555"

red :: RGBA
red = hexToRGBA redHex

yellowHex :: Hex
yellowHex = "#f1fa8c"

yellow :: RGBA
yellow = hexToRGBA yellowHex
