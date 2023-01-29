module Berks.Hexes
  ( RGB,
    RGBA,
    hexToRGB,
    addAlphaToRGB,
    hexToRGBWithAlpha,
    hexToRGBA,
  )
where

import Numeric (readHex)

type RGB = (Double, Double, Double)

type RGBA = (Double, Double, Double, Double)

splitIntoComps :: String -> [String]
splitIntoComps [] = []
splitIntoComps [_] =
  error "Expected Hex an even number of chars but found a odd number"
splitIntoComps (x : y : xs) = [x, y] : splitIntoComps xs

hexToFloat :: String -> Double
hexToFloat hx = case readHex hx of
  (x, _) : _ -> x / 255
  _ -> error $ "Couldn't parse hex " ++ hx ++ " to Double"

hexToRGB :: String -> RGB
hexToRGB hex = case length hex of
  7 -> (hexToFloat r, hexToFloat g, hexToFloat b)
    where
      [r, g, b] = splitIntoComps $ drop 1 hex
  6 -> (hexToFloat r, hexToFloat g, hexToFloat b)
    where
      [r, g, b] = splitIntoComps hex
  _ ->
    error $
      "Expected Hex to be 7 or 6 chars long but found "
        ++ show
          (length hex)

addAlphaToRGB :: RGB -> Double -> RGBA
addAlphaToRGB (r, g, b) alpha = (r, g, b, alpha)

hexToRGBWithAlpha :: String -> Double -> RGBA
hexToRGBWithAlpha hex = addAlphaToRGB $ hexToRGB hex

hexToRGBA :: String -> RGBA
hexToRGBA hex = hexToRGBWithAlpha hex 1
