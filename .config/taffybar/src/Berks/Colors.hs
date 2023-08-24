module Berks.Colors
  ( transparent,
    rgbas,
    hexes,
    Hex,
    C (..),
  )
where

import Berks.Hexes

type Hex = String

data C a = C
  { rosewater :: a,
    flamingo :: a,
    pink :: a,
    mauve :: a,
    red :: a,
    maroon :: a,
    peach :: a,
    yellow :: a,
    green :: a,
    teal :: a,
    sky :: a,
    sapphire :: a,
    blue :: a,
    lavender :: a,
    text :: a,
    subtext1 :: a,
    subtext0 :: a,
    overlay2 :: a,
    overlay1 :: a,
    overlay0 :: a,
    surface2 :: a,
    surface1 :: a,
    surface0 :: a,
    base :: a,
    mantle :: a,
    crust :: a
  }
  deriving (Show, Eq)

-- add C a to Functor
instance Functor C where
  fmap f c =
    C
      { rosewater = f $ rosewater c,
        flamingo = f $ flamingo c,
        pink = f $ pink c,
        mauve = f $ mauve c,
        red = f $ red c,
        maroon = f $ maroon c,
        peach = f $ peach c,
        yellow = f $ yellow c,
        green = f $ green c,
        teal = f $ teal c,
        sky = f $ sky c,
        sapphire = f $ sapphire c,
        blue = f $ blue c,
        lavender = f $ lavender c,
        text = f $ text c,
        subtext1 = f $ subtext1 c,
        subtext0 = f $ subtext0 c,
        overlay2 = f $ overlay2 c,
        overlay1 = f $ overlay1 c,
        overlay0 = f $ overlay0 c,
        surface2 = f $ surface2 c,
        surface1 = f $ surface1 c,
        surface0 = f $ surface0 c,
        base = f $ base c,
        mantle = f $ mantle c,
        crust = f $ crust c
      }

hexes :: C Hex
hexes =
  C
    { rosewater = "#f2d5cf",
      flamingo = "#eebebe",
      pink = "#f4b8e4",
      mauve = "#ca9ee6",
      red = "#e78284",
      maroon = "#ea999c",
      peach = "#ef9f76",
      yellow = "#e5c890",
      green = "#a6d189",
      teal = "#81c8be",
      sky = "#99d1db",
      sapphire = "#85c1dc",
      blue = "#8caaee",
      lavender = "#babbf1",
      text = "#c6d0f5",
      subtext1 = "#b5bfe2",
      subtext0 = "#a5adce",
      overlay2 = "#949cbb",
      overlay1 = "#838ba7",
      overlay0 = "#737994",
      surface2 = "#626880",
      surface1 = "#51576d",
      surface0 = "#414559",
      base = "#303446",
      mantle = "#292c3c",
      crust = "#232634"
    }

rgbas :: C RGBA
rgbas = fmap hexToRGBA hexes

transparent :: RGBA
transparent = (0, 0, 0, 0)
