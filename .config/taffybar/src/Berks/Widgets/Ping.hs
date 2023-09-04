module Berks.Widgets.Ping
  ( connectivityWidget,
  )
where

import Berks.Colors (C (..), hexes)
import Data.Text
import GI.Gtk (Widget)
import System.Log.Logger (errorM)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Util (runCommand)
import System.Taffybar.Widget (colorize)
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import Text.Printf (printf)
import Prelude

connectedIcon :: String
connectedIcon = "\xf0318" -- 󰌘

disconnectedIcon :: String
disconnectedIcon = "\xf127" -- 

pingCmd :: FilePath
pingCmd = "ping"

pingTimeout :: String
pingTimeout = "3"

routeToPing :: String
routeToPing = "1.1.1.1" -- Cloudflare DNS

pingCmdRunner :: [String] -> IO Bool
pingCmdRunner args = runCommand pingCmd args >>= either logError (return . const True)
  where
    logError err = errorM "Berks.Widgets.Ping" (printf "Got error in CommandRunner %s" err) >> return False

isOnline :: IO Text
isOnline = do
  isConnected <- pingCmdRunner ["-c", "1", "-w", pingTimeout, routeToPing]
  let icon = if isConnected then connectedIcon else disconnectedIcon
  return $ pack $ colorize (if isConnected then sky hexes else red hexes) "" icon <> " "

connectivityWidget :: TaffyIO Widget
connectivityWidget = pollingLabelNew 1 isOnline
