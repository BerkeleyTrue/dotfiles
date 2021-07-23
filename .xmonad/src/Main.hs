{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import qualified Data.Semigroup as SG

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- Actions
import qualified XMonad.Actions.CycleWS as CWs
import XMonad.Actions.WindowNavigation

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

-- layout modifiers
import XMonad.Layout.MultiToggle as Mt (Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))
import XMonad.Layout.Reflect as LR
import XMonad.Layout.WindowNavigation

import qualified Berks.Colors as Cl
import qualified Berks.GridSelect as GS
import qualified Berks.Layouts.Main as L

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
-- Key bindings. Add, modify or remove key bindings here.
--
keyMaps :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keyMaps XConfig {modMask = modm, layoutHook = layoutHk} =
  M.fromList $
  -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn term)
  -- launch dmenu
  , ((modm, xK_p), spawn "dmenu_run")
  -- launch rofi
  , ((modm, xK_d), spawn "rofi -show drun -m 1")
  , ((modm, xK_g), GS.createAppGridSpawner ())
  -- close focused window
  , ((modm .|. shiftMask, xK_q), kill)
  -- Rotate through Screens
  , ((modm, xK_Tab), CWs.nextScreen)
    -- Rotate through the available layouts
  , ((modm .|. shiftMask, xK_Tab), sendMessage NextLayout)
  --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space), setLayout layoutHk)
    -- switch to next monitor
  , ((modm, xK_f), sendMessage (Mt.Toggle NBFULL) >> sendMessage ToggleStruts)
  -- Move focus to the next window
  , ((modm, xK_j), windows W.focusDown)
  -- Move focus to the previous window
  , ((modm, xK_k), windows W.focusUp)
  -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)
  -- Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_m), windows W.swapMaster)
  -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm .|. controlMask, xK_y), sendMessage $ Toggle LR.REFLECTY)
  , ((modm .|. controlMask, xK_x), sendMessage $ Toggle LR.REFLECTX)
  -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)
  -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)
  -- Drown window
  , ((modm, xK_t), withFocused $ windows . W.sink)
  -- Directional Nav
  , ((modm, xK_Right), sendMessage $ Go R)
  , ((modm, xK_Left), sendMessage $ Go L)
  , ((modm, xK_Up), sendMessage $ Go U)
  , ((modm, xK_Down), sendMessage $ Go D)
  , ((modm .|. shiftMask, xK_Right), sendMessage $ Swap R)
  , ((modm .|. shiftMask, xK_Left), sendMessage $ Swap L)
  , ((modm .|. shiftMask, xK_Up), sendMessage $ Swap U)
  , ((modm .|. shiftMask, xK_Down), sendMessage $ Swap D)
    -- Restart xmonad
  , ( (modm .|. shiftMask, xK_r)
    , spawn
        "\
      \ notify-send -a 'XMonad' 'Recompiling...' && \
      \ xmonad --recompile && \
      \ xmonad --restart && \
      \ notify-send -a 'XMonad'  'Restarted'")
    -- quit menu
  , ((modm, xK_0), spawn "$HOME/.local/bin/powermenu")
  ] ++
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip werkspaces [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

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
    ]

------------------------------------------------------------------------
-- Event handling
myEventHook :: Event -> X SG.All
myEventHook = fadeWindowsEventHook <> fullscreenEventHook

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
  spawnOnce "nitrogen --restore &" <>
  spawnOnce "picom -b --experimental-backends &" <>
  spawnOnce "flameshot &" <>
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
enhanceXConf :: XConfig a -> XConfig a
enhanceXConf = docks . ewmh

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc0.hs"
  xmproc1 <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc1.hs"
  xmonad $
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
        , keys = keyMaps
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
                , ppVisible = xmobarColor Cl.comment "" . wrap "(" ")"
                , ppTitle = xmobarColor Cl.purple "" . pad . shorten 20
                , ppLayout = xmobarColor Cl.red "" . wrap "<" ">"
                , ppSep = " "
                , ppOrder = \(ws:l:t:_) -> [ws, t, l]
                }
        }
