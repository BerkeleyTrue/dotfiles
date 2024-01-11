{-# OPTIONS_GHC -Wall #-}

import Berks.CheatSheet as CheatSh
import Berks.Colors
import Berks.KeyMaps
import Berks.Layouts.Main as L
import Berks.MultiToggleState
  ( createToggleStateLogHook,
    toggleRefIO,
  )
import Berks.Scratchpads
import Berks.Urgency
import Berks.Utils
import Data.Map
import Data.Semigroup
import XMonad
import XMonad.Actions.TagWindows
  ( addTag,
    delTag,
  )
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ScreenCorners
  ( ScreenCorner (SCLowerRight),
    addScreenCorners,
    screenCornerEventHook,
  )
import XMonad.Hooks.SetWMName
import XMonad.Hooks.TaffybarPagerHints
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WorkspaceHistory
import XMonad.Prelude (bool)
import XMonad.StackSet hiding
  ( focus,
    member,
    workspaces,
  )
import XMonad.Util.NamedActions
import XMonad.Util.SpawnOnce (spawnOnce)

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
mBindings :: XConfig l -> Map (KeyMask, Button) (Window -> X ())
mBindings XConfig {XMonad.modMask = modm} =
  fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows shiftMaster),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> focus w >> windows shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w -> focus w >> mouseResizeWindow w >> windows shiftMaster
      )
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
-- NOTE: --> is an infix 0 function (predicate -> action)
--   If the predicate is true do the action
--   predicate and action are wrapped in monoids
-- NOTE: =? is a query class
-- NOTE: Useful here is <&&> which is infix op, same as && (AND) but for wrapped monoids
-- NOTE: Useful here is <||> which is infix op, same as || (OR) but for wrapped monoids
-- NOTE: <+> composes two monoids
myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ appName =? "desktop_window" --> doIgnore,
      appName =? "kdesktop" --> doIgnore,
      isFullscreen --> doFullFloat,
      className =? "Slack" --> doShift "3",
      className =? "discord" --> doShift "3",
      className =? "Zenity" --> doRectFloat CheatSh.size,
      className =? "Yad" --> doRectFloat CheatSh.size,
      className =? "Xdg-desktop-portal-gtk" --> doRectFloat centerWindow,
      className =? "Blueman-manager" --> doRectFloat centerWindow,
      className =? ".blueman-manager-wrapped" --> doRectFloat centerWindow, -- nix wrapped blueman
      className =? "nm-connection-editor" --> doRectFloat centerWindow,
      className =? "Nm-connection-editor" --> doRectFloat centerWindow,
      className =? "Peek" --> doRectFloat centerWindow
    ]
    <+> scratchpadManageHook

------------------------------------------------------------------------
-- Event handling
myEventHook :: Event -> X All
myEventHook = screenCornerEventHook

------------------------------------------------------------------------
-- Log hook actions are triggered with any change in the window state by XMonad
--
-- workspaceHistoryHook -> A logHook that keeps track of the order in which workspaces have been viewed.
-- fadeWindowsLogHook -> A logHook to fade windows under control of a FadeHook, which is similar to but not identical to ManageHook.
-- tagHook -> Add floating tag to windows in the floating layer on every change
myLogHook :: X ()
myLogHook = workspaceHistoryHook >> tagHook
  where
    toggleTagOn = bool delTag addTag
    isWindowInFloatingSet windowSet window = member window $ floating windowSet
    toggleFloatingTag windowSet window =
      toggleTagOn (isWindowInFloatingSet windowSet window) "floating" window

    tagHook = withWindowSet $ mapM_ <$> toggleFloatingTag <*> allWindows

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook =
  setWMName "LG3D"
    -- we use systemd to manage most things since a lot of sni stuff has delicate timing
    <> spawnOnce "systemctl --no-block --user start xmonad.target"
    <> addScreenCorners [(SCLowerRight, spawn "$HOME/.local/bin/lock")]

------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Starting XMonad"
  toggleDataRef <- toggleRefIO
  let logHook' = createToggleStateLogHook toggleDataRef >> myLogHook
      keyBindings = createKeyMaps term werkspaces toggleDataRef

  xmonad
    $ addDescrKeys'
      ((myModMask .|. shiftMask, xK_slash), CheatSh.cheatSheetView)
      keyBindings
    $ enhanceXConf
      def
        { terminal = term,
          -- Whether focus follows the mouse pointer.
          focusFollowsMouse = True,
          -- Whether clicking on a window to focus also passes the click to the window
          clickJustFocuses = False,
          -- Width of the window border in pixels.
          borderWidth = 4,
          modMask = myModMask,
          workspaces = werkspaces,
          -- Border colors for unfocused and focused windows, respectively.
          normalBorderColor = base hexes,
          focusedBorderColor = rosewater hexes,
          -- bindings
          mouseBindings = mBindings,
          -- hooks, layouts
          layoutHook = L.layout,
          manageHook = myManageHook,
          handleEventHook = myEventHook,
          startupHook = myStartupHook,
          logHook = logHook'
        }
  where
    enhanceXConf =
      withUrgencyHook UrgencyHookInstance . docks . ewmh . pagerHints
