module Berks.Urgency
  ( UrgencyHookInstance(..)
  ) where

import XMonad
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows
import XMonad.Util.Run

---------------------------------------------------------------------------
-- Urgency Hook:
--
-- Allows you to use notifications for xmonad
-- thanks to https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
---------------------------------------------------------------------------
data UrgencyHookInstance =
  UrgencyHookInstance
  deriving (Read, Show)

instance UrgencyHook UrgencyHookInstance where
  urgencyHook UrgencyHookInstance w = do
    name <- getName w
    Just idx <- W.findTag w <$> gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]
