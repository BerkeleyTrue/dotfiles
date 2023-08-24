module Berks.Widgets.Crypto
  ( ethWidget,
    btcWidget,
    ohmWidget,
    xtzWidget,
    pickleWidget,
    spyWidget,
  )
where

import Berks.Colors
  ( hexes,
    sapphire,
    green,
    pink,
    lavender,
    red,
  )
import Berks.WidgetUtils (runCommandWithDefault)
import Data.Text
import GI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )
import System.Taffybar.Widget.Util (colorize)

padLeft :: String -> String
padLeft = (" " <>)

padRight :: String -> String
padRight = (<> " ")

-- NOTE: make sure this is in your path
crypoEggcmd :: FilePath
crypoEggcmd = "crypto-egg-go"

eggCommandRunner :: [String] -> IO String
eggCommandRunner args = runCommandWithDefault crypoEggcmd args "0.xx"

ethPrice :: IO String
ethPrice = eggCommandRunner ["price", "eth"]

ethGas :: IO String
ethGas = eggCommandRunner ["gas"]

flippening :: IO String
flippening = eggCommandRunner ["flip"]

btcPrice :: IO String
btcPrice = eggCommandRunner ["price", "btc"]

ohmPrice :: IO String
ohmPrice = eggCommandRunner ["price", "ohm"]

xtzPrice :: IO String
xtzPrice = eggCommandRunner ["price", "xtz"]

picklePrice :: IO String
picklePrice = eggCommandRunner ["price", "pickle"]

spyPrice :: IO String
spyPrice = runCommandWithDefault "spy" [] "0.xx"

label :: IO Text
label = do
  eth <- ethPrice
  ethGas' <- ethGas
  flip' <- flippening
  return
    . pack
    $ colorize (sapphire hexes) "" ("\xf086a " <> eth <> " ")
      <> colorize (green hexes) "" "\xf0298 "
      <> colorize (sapphire hexes) "" ethGas'
      <> " "
      <> (colorize (red hexes) "" "\xf0450 " <> colorize (sapphire hexes) "" flip' <> " ")

ethWidget :: TaffyIO Widget
ethWidget = pollingLabelNew 1 label

btcWidget :: TaffyIO Widget
btcWidget =
  pollingLabelNew 1 $
    pack
      . colorize (green hexes) ""
      . padRight
      . ("\xf01ac" <>)
      . padLeft
      <$> btcPrice

ohmWidget :: TaffyIO Widget
ohmWidget =
  pollingLabelNew 1 $
    pack
      . colorize (lavender hexes) ""
      . padRight
      . ("\xf03c9" <>)
      . padLeft
      <$> ohmPrice

xtzWidget :: TaffyIO Widget
xtzWidget =
  pollingLabelNew 1 $
    pack
      . colorize (red hexes) ""
      . padRight
      . ("XTZ" <>)
      . padLeft
      <$> xtzPrice

pickleWidget :: TaffyIO Widget
pickleWidget =
  pollingLabelNew 1 $
    pack
      . colorize (green hexes) ""
      . padRight
      . ("PICKLE" <>)
      . padLeft
      <$> picklePrice

spyWidget :: TaffyIO Widget
spyWidget =
  pollingLabelNew 1 $
    pack
      . colorize (pink hexes) ""
      . padRight
      . ("\xf11eb" <>)
      . padLeft
      <$> spyPrice
