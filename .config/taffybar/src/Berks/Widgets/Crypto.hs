module Berks.Widgets.Crypto
  ( ethWidget,
  )
where

import Berks.Colors
  ( cyanHex,
    greenHex,
    pinkHex,
    redHex,
  )
import Berks.WidgetUtils (runCommandWithDefault)
import Data.Text
import GI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Generic.PollingLabel
  ( pollingLabelNew,
  )
import System.Taffybar.Widget.Util (colorize)

crypoEggcmd :: FilePath
crypoEggcmd = "/home/berkeleytrue/.local/bin/crypto-egg-go"

eggCommandRunner :: [String] -> IO String
eggCommandRunner args = runCommandWithDefault crypoEggcmd args "0.xx"

-- commandRunner = runCommandWithDefault crypoEggcmd ["price", "eth"] "0.xx"

ethPrice :: IO String
ethPrice = eggCommandRunner ["price", "eth"]

ethGas :: IO String
ethGas = eggCommandRunner ["gas"]

stEthPrice :: IO String
stEthPrice = eggCommandRunner ["steth"]

flippening :: IO String
flippening = eggCommandRunner ["flip"]

label :: IO Text
label = do
  eth <- ethPrice
  ethGas' <- ethGas
  stEth <- stEthPrice
  flip' <- flippening
  return
    . pack
    $ colorize cyanHex "" ("\xf086a " <> eth <> " ")
      <> colorize greenHex "" "\xf0298 "
      <> colorize cyanHex "" ethGas'
      <> " "
      <> (colorize redHex "" "\xf0450 " <> colorize cyanHex "" flip' <> " ")
      <> colorize pinkHex "" "\xe62d "
      <> colorize cyanHex "" stEth

ethWidget :: TaffyIO Widget
ethWidget = pollingLabelNew 1 label
