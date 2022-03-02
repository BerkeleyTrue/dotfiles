{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import qualified Data.Semigroup as SG

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedActions
import XMonad.Util.Run

-- Actions
import qualified XMonad.Actions.CycleWS as CWs
import XMonad.Actions.WindowNavigation

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.UrgencyHook

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

-- layout modifiers
import XMonad.Layout.MultiToggle as Mt (Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))
import XMonad.Layout.Reflect as LR
import XMonad.Layout.WindowNavigation

import qualified Berks.CheatSheet as CheatSh
import qualified Berks.Colors as Cl
import qualified Berks.GridSelect as GS
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
-- Key bindings. Add, modify or remove key bindings here.
-- NOTE: We are deconstructuring XConfig below (i.e. Type {property = <local symbol>})
keyMaps :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
keyMaps XConfig {modMask = modm, layoutHook = layoutHk} =
  [ ( (modm .|. shiftMask, xK_Return)
    , addName ("Launch " ++ term ++ " terminal") $ spawn term)
  --
  , ((modm, xK_p), addName "Launch dmenu" $ spawn "dmenu_run")
  --
  , ((modm, xK_d), addName "Launch rofi" $ spawn "rofi -show drun")
  --
  , ((modm, xK_s), addName "Launch Spell" $ spawn "$HOME/.local/bin/rofi-spell")
  --
  , ( (modm, xK_0)
    , addName "Open power menu" $ spawn "$HOME/.local/bin/powermenu")
  --
  , ((modm, xK_g), addName "Launch grid selector" $ GS.createAppGridSpawner ())
  --
  , ((modm .|. shiftMask, xK_q), addName "close focused window" kill)
  --
  , ((modm, xK_Tab), addName "Rotate through Screens" CWs.nextScreen)
  --
  , ( (modm .|. shiftMask, xK_Tab)
    , addName "Rotate through the available layouts" $ sendMessage NextLayout)
  --
  , ( (modm .|. shiftMask, xK_space)
    , addName "Reset the layouts on the current workspace to default" $
      setLayout layoutHk)
  --
  , ( (modm, xK_f)
    , addName "Switch layout to full screen no topbar" $
      sendMessage (Mt.Toggle NBFULL) >> sendMessage ToggleStruts)
  --
  , ( (modm, xK_j)
    , addName "Move focus to the next window" $ windows W.focusDown)
  --
  , ( (modm, xK_k)
    , addName "Move focus to the previous window" $ windows W.focusUp)
  --
  , ( (modm, xK_m)
    , addName "Move focus to the master window" $ windows W.focusMaster)
  , ( (modm .|. shiftMask, xK_m)
    , addName "Swap the focused window and the master window" $
      windows W.swapMaster)
  --
  , ( (modm .|. shiftMask, xK_j)
    , addName "Swap the focused window with the next window" $
      windows W.swapDown)
  --
  , ( (modm .|. shiftMask, xK_k)
    , addName "Swap the focused window with the previous window" $
      windows W.swapUp)
  , ( (modm .|. controlMask, xK_y)
    , addName "Flip layout on Y axis" $ sendMessage $ Toggle LR.REFLECTY)
  , ( (modm .|. controlMask, xK_x)
    , addName "Flip layout on X axis" $ sendMessage $ Toggle LR.REFLECTX)
  --
  , ((modm, xK_h), addName "Shrink the master area" $ sendMessage Shrink)
  --
  , ((modm, xK_l), addName "Expand the master area" $ sendMessage Expand)
  --
  , ( (modm, xK_t)
    , addName "Sink window into layout" $ withFocused $ windows . W.sink)
  --
  , ((modm, xK_Right), addName "Go Right" $ sendMessage $ Go R)
  , ((modm, xK_Left), addName "Go Left" $ sendMessage $ Go L)
  , ((modm, xK_Up), addName "Go Up" $ sendMessage $ Go U)
  , ((modm, xK_Down), addName "Go down" $ sendMessage $ Go D)
  --
  , ( (modm .|. shiftMask, xK_Right)
    , addName "Swap Right" $ sendMessage $ Swap R)
  , ((modm .|. shiftMask, xK_Left), addName "Swap Left" $ sendMessage $ Swap L)
  , ((modm .|. shiftMask, xK_Up), addName "Swap Up" $ sendMessage $ Swap U)
  , ((modm .|. shiftMask, xK_Down), addName "Swap Down" $ sendMessage $ Swap D)
  --
  , ( (modm .|. shiftMask, xK_r)
    , addName "Restart XMonad" $
      spawn
        "\
      \ xmonad --restart && \
      \ notify-send -a 'XMonad'  'Restarted'")
  , ( (modm .|. controlMask .|. shiftMask, xK_r)
    , addName "Recompile and Restart xmonad" $
      spawn
        "\
      \ notify-send -a 'XMonad' 'Recompiling...' && \
      \ xmonad --recompile && \
      \ xmonad --restart && \
      \ notify-send -a 'XMonad'  'Restarted'")
  ] ++
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  -- keyMask here is either 0 or shiftMask
  -- xAction is either W.greedyView or W.shift
  -- werkSpace is the workspace name
  -- keySym is the symbol for the keys 0 - 9
  -- we take the array of [(StackSet, KeyMask|void)]
  -- and an array of [werkspace, keySym] and zip them together then map to
  -- [(shiftMask .|. modm, keySym), NamedAction<Swift>)]
  -- [(modm, keySym), NamedAction<GreedyView>)]
  [ ( (keyMask .|. modm, keySym)
    , addName (name ++ werkSpace ++ " workspace") $ windows $ xAction werkSpace)
  | (werkSpace, keySym) <- zip werkspaces [xK_1 .. xK_9]
  , (xAction, keyMask, name) <-
      [(W.greedyView, 0, "Switch To "), (W.shift, shiftMask, "Move To ")]
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
      keyMaps $
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
