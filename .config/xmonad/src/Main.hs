{-# OPTIONS_GHC -Wall #-}

import Berks.CheatSheet as CheatSh
import Berks.Colors
import Berks.KeyMaps
import Berks.Layouts.Main as L
import Berks.Scratchpads
import Berks.Urgency
import Berks.Utils
import Data.Map
import Data.Semigroup
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WorkspaceHistory
import XMonad.StackSet hiding
  ( focus,
    workspaces,
  )
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

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
    [ resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore,
      isFullscreen --> doFullFloat,
      className =? "Slack" --> doShift "3",
      className =? "discord" --> doShift "3",
      className =? "Zenity" --> doRectFloat CheatSh.size,
      className =? "Yad" --> doRectFloat CheatSh.size,
      className =? "Xdg-desktop-portal-gtk" --> doRectFloat centerWindow,
      className =? "Blueman-manager" --> doRectFloat centerWindow
    ]
    <+> scratchpadManageHook

------------------------------------------------------------------------
-- Event handling
-- fadeWindowsEventHook -> A handleEventHook to handle fading and unfading of newly mapped or unmapped windows;
myEventHook :: Event -> X All
myEventHook = fadeWindowsEventHook

------------------------------------------------------------------------
-- Status bars and logging
-- workspaceHistoryHook -> A logHook that keeps track of the order in which workspaces have been viewed.
-- fadeWindowsLogHook -> A logHook to fade windows under control of a FadeHook, which is similar to but not identical to ManageHook.
myLogHook :: X ()
myLogHook = workspaceHistoryHook >> fadeWindowsLogHook myFadeHook
  where
    myFadeHook = composeAll [opaque, isUnfocused --> transparency 0.02]

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
    <> spawn "killall trayer &> /dev/null"
    <> spawn "sleep 1 && source $HOME/.config/screenlayout/default.sh"
    <> spawn
      "sleep 2 && \
      \ trayer --edge top \
      \ --align right \
      \ --widthtype request \
      \ --padding 6 \
      \ --SetDockType true \
      \ --SetPartialStrut false \
      \ --expand true \
      \ --monitor 0 \
      \ --transparent true \
      \ --alpha 0 \
      \ --tint 0x282c34 \
      \ --iconspacing 6 \
      \ --height 22 &"

------------------------------------------------------------------------
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc0.hs"
  xmproc1 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc1.hs"
  xmonad $
    withUrgencyHook UrgencyHookInstance $
      addDescrKeys'
        ((myModMask .|. shiftMask, xK_slash), CheatSh.cheatSheetView)
        (createKeyMaps term werkspaces)
        $ enhanceXConf
          def
            { terminal = term,
              -- Whether focus follows the mouse pointer.
              focusFollowsMouse = True,
              -- Whether clicking on a window to focus also passes the click to the window
              clickJustFocuses = True,
              -- Width of the window border in pixels.
              borderWidth = 1,
              modMask = myModMask,
              workspaces = werkspaces,
              -- Border colors for unfocused and focused windows, respectively.
              normalBorderColor = background,
              focusedBorderColor = cyan,
              -- bindings
              mouseBindings = mBindings,
              -- hooks, layouts
              layoutHook = L.layout,
              manageHook = myManageHook,
              handleEventHook = myEventHook,
              startupHook = myStartupHook,
              logHook =
                myLogHook
                  <> createPPLog
                    xmobarPP
                      { -- outputs of the entire bar
                        -- input from xmonad gets sent to xmonad proc 0,
                        -- then the output from that gets sent into hPutStrLn
                        -- and then into xmonad to be displayed
                        ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x,
                        ppCurrent = xmobarColor cyan "" . wrap "[" "]",
                        ppVisible = xmobarColor comment "" . wrap "[[" "]]",
                        ppTitle = xmobarColor purple "" . pad . shorten 20,
                        ppLayout = xmobarColor red "" . wrap "<" ">",
                        ppSep = " ",
                        ppOrder = \(ws : l : t : _) -> [ws, t, l]
                      }
            }
  where
    enhanceXConf = docks . ewmh
    createPPLog = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag]
