module Berks.Colors
  ( background,
    selection,
    foreground,
    comment,
    cyan,
    green,
    orange,
    pink,
    purple,
    red,
    yellow,
  )
where

import Berks.Hexes

background :: RGBA
background = hexToRGBA "#282a36"

selection :: RGBA
selection = hexToRGBA "#44475a"

foreground :: RGBA
foreground = hexToRGBA "#f8f8f2"

comment :: RGBA
comment = hexToRGBA "#6272a4"

cyan :: RGBA
cyan = hexToRGBA "#8be9fd"

green :: RGBA
green = hexToRGBA "#50fa7b"

orange :: RGBA
orange = hexToRGBA "#ffb86c"

pink :: RGBA
pink = hexToRGBA "#ff79c6"

purple :: RGBA
purple = hexToRGBA "#bd93f9"

red :: RGBA
red = hexToRGBA "#ff5555"

yellow :: RGBA
yellow = hexToRGBA "#f1fa8c"
