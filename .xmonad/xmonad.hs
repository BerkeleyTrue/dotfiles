import qualified Data.Map as M
import Data.Monoid
import System.Exit

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- Actions
import qualified XMonad.Actions.CycleWS as CWs
import XMonad.Actions.MouseResize
import XMonad.Actions.WindowNavigation

-- Hooks
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

-- layouts
import XMonad.Layout.Magnifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

-- layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.MultiToggle as Mt
  ( EOT(EOT)
  , Toggle(Toggle)
  , (??)
  , mkToggle
  )
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

import qualified Berks.Colors as Cl
import qualified Berks.Font as Fs
import qualified Berks.GridSelect as GS

-- Terminal
term = "kitty"

-- Super key as Mod
myModMask = mod4Mask

-- Workspaces
werkspace = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
keyMaps conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
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
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- switch to next monitor
  , ((modm, xK_f), sendMessage (Mt.Toggle NBFULL) >> sendMessage ToggleStruts)
  -- Move focus to the next window
  , ((modm, xK_j), windows W.focusDown)
  -- Move focus to the previous window
  , ((modm, xK_k), windows W.focusUp)
  -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)
  -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)
  -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
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
        "xmonad --recompile && xmonad --restart && notify-send 'XMonad Restarted'")
    -- quit menu
  , ((modm, xK_0), spawn "$HOME/.local/bin/powermenu")
  ] ++
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
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
-- Layouts:
tabTheme =
  def
    { fontName = Fs.font
    , inactiveTextColor = Cl.comment
    , inactiveColor = Cl.background
    , inactiveBorderColor = Cl.background
    , activeTextColor = Cl.foreground
    , activeColor = Cl.cyan
    , activeBorderColor = Cl.cyan
    }

--Makes setting the spacingRaw simpler to write.
--The spacingRaw module adds a configurable amount of space around windows.
mySpacing ::
     Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

magnify =
  renamed [Replace "Magnify"] $
  windowNavigation $
  addTabs shrinkText tabTheme $
  subLayout [] (smartBorders Simplest) $
  magnifier $
  limitWindows 12 $ mySpacing 8 $ ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "Monocle"] $
  windowNavigation $
  addTabs shrinkText tabTheme $
  subLayout [] (smartBorders Simplest) $ limitWindows 20 Full

vert = renamed [Replace "Vert"] $ windowNavigation $ Tall 1 (3 / 100) (1 / 2)

horiz = renamed [Replace "Horiz"] $ Mirror vert

myLayout =
  avoidStruts . mouseResize . windowArrange $
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) layouts
  where
    layouts = magnify ||| vert ||| monocle ||| horiz

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
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , resource =? "desktop_window" --> doIgnore
    , resource =? "kdesktop" --> doIgnore
    , isFullscreen --> doFullFloat
    ]

------------------------------------------------------------------------
-- Event handling
myEventHook = (<>) fadeWindowsEventHook fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging
myLogHook :: X ()
myLogHook = fadeWindowsLogHook myFadeHook
  where
    myFadeHook = composeAll [isUnfocused --> transparency 0.5, opaque]

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom -b &"
  spawnOnce "flameshot &"
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
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar  $HOME/.config/xmobar/xmobarrc0.hs"
  xmonad $
    docks $
    ewmh
      def
        { terminal = term
        -- Whether focus follows the mouse pointer.
        , focusFollowsMouse = True
        -- Whether clicking on a window to focus also passes the click to the window
        , clickJustFocuses = True
        -- Width of the window border in pixels.
        , borderWidth = 1
        , modMask = myModMask
        , workspaces = werkspace
        -- Border colors for unfocused and focused windows, respectively.
        , normalBorderColor = Cl.background
        , focusedBorderColor = Cl.cyan
        -- bindings
        , keys = keyMaps
        , mouseBindings = mBindings
        -- hooks, layouts
        , layoutHook = myLayout
        , manageHook = myManageHook
        , handleEventHook = myEventHook
        , startupHook = myStartupHook
        , logHook =
            workspaceHistoryHook <+>
            myLogHook <+>
            dynamicLogWithPP
              xmobarPP
                -- outputs of the entire bar
                -- input from xmonad gets sent to xmonad proc 0,
                -- then the output from that gets sent into hPutStrLn
                -- and then into xmonad to be displayed
                { ppOutput = hPutStrLn xmproc0
                , ppCurrent = xmobarColor Cl.cyan "" . wrap "[" "]"
                , ppVisible = xmobarColor Cl.comment "" . wrap "(" ")"
                , ppTitle = xmobarColor Cl.purple "" . pad . shorten 20
                , ppLayout = xmobarColor Cl.red "" . wrap "<" ">"
                , ppSep = " "
                , ppOrder = \(ws:l:t:_) -> [ws, t, l]
                }
        }
