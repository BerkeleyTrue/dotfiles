{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import qualified Data.Semigroup as SG

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedActions
import XMonad.Util.Run

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops as Ewmh (ewmh)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, doRectFloat, isFullscreen)
import XMonad.Hooks.UrgencyHook

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

-- layout modifiers
import qualified Berks.CheatSheet as CheatSh
import qualified Berks.Colors as Cl
import qualified Berks.KeyMaps as KMaps
import qualified Berks.Layouts.Main as L
import qualified Berks.Urgency as Urg

-- Terminal
term :: String
term = "kitty"

-- Super key as Mod
myModMask :: KeyMask
myModMask = mod4Mask

-- Workspaces
werkspaces :: [String]
werkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
mBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
mBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ( (modm, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- ex: className =? "Slack" --> doShift (werkspaces !! 1)
myManageHook :: Query (SG.Endo WindowSet)
myManageHook =
  composeAll
    [ resource =? "desktop_window" --> doIgnore
    , resource =? "kdesktop" --> doIgnore
    , isFullscreen --> doFullFloat
    , className =? "Slack" --> doShift "3"
    , className =? "discord" --> doShift "3"
    , className =? "zoom" --> doShift "4"
    , className =? "Zenity" --> doRectFloat CheatSh.size
    ]

------------------------------------------------------------------------
-- Event handling
myEventHook :: Event -> X SG.All
myEventHook = fadeWindowsEventHook

------------------------------------------------------------------------
-- Status bars and logging
myLogHook :: X ()
myLogHook = workspaceHistoryHook >> fadeWindowsLogHook myFadeHook
  where
    myFadeHook = composeAll [isUnfocused --> transparency 0.5, opaque]

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook =
  setWMName "LG3D" <>
  spawn "source $HOME/.config/screenlayout/default.sh" <>
  spawn "nitrogen --restore" <>
  spawn
    "killall trayer; \
    \trayer --edge top \
    \--align right \
    \--widthtype request \
    \--padding 6 \
    \--SetDockType true \
    \--SetPartialStrut false \
    \--expand true \
    \--monitor 0 \
    \--transparent true \
    \--alpha 0 \
    \--tint 0x282c34 \
    \--height 18 &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
--
-- Keybinding to display the keybinding cheatsheet
-- myCheatsheetKey conf = addDescrKeys' (myModMask .|. shiftMask, xK_slash) keyMaps conf
enhanceXConf :: XConfig a -> XConfig a
enhanceXConf = docks . ewmh

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc0.hs"
  xmproc1 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc1.hs"
  xmonad $
    withUrgencyHook Urg.UrgencyHookInstance $
    addDescrKeys'
      ((myModMask .|. shiftMask, xK_slash), CheatSh.cheatSheetView)
      (KMaps.createKeyMaps term werkspaces) $
    enhanceXConf
      def
        { terminal = term
        -- Whether focus follows the mouse pointer.
        , focusFollowsMouse = True
        -- Whether clicking on a window to focus also passes the click to the window
        , clickJustFocuses = True
        -- Width of the window border in pixels.
        , borderWidth = 1
        , modMask = myModMask
        , workspaces = werkspaces
        -- Border colors for unfocused and focused windows, respectively.
        , normalBorderColor = Cl.background
        , focusedBorderColor = Cl.cyan
        -- bindings
        , mouseBindings = mBindings
        -- hooks, layouts
        , layoutHook = L.layout
        , manageHook = myManageHook
        , handleEventHook = myEventHook
        , startupHook = myStartupHook
        , logHook =
            myLogHook <>
            dynamicLogWithPP
              xmobarPP
                -- outputs of the entire bar
                -- input from xmonad gets sent to xmonad proc 0,
                -- then the output from that gets sent into hPutStrLn
                -- and then into xmonad to be displayed
                { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                , ppCurrent = xmobarColor Cl.cyan "" . wrap "[" "]"
                , ppVisible = xmobarColor Cl.comment "" . wrap "[[" "]]"
                , ppTitle = xmobarColor Cl.purple "" . pad . shorten 20
                , ppLayout = xmobarColor Cl.red "" . wrap "<" ">"
                , ppSep = " "
                , ppOrder = \(ws:l:t:_) -> [ws, t, l]
                }
        }
