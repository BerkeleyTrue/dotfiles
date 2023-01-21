module Berks.KeyMaps
  ( createKeyMaps,
    StrutState (Default, FullScreen, SingleWindow),
  )
where

-- layout modifiers

import Berks.GridSelect
import Berks.Scratchpads
import Data.IORef
import Data.Ratio as Ratio
  ( (%),
  )
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp
import XMonad.Core
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.WindowNavigation
  ( Direction2D (D, L, R, U),
    Navigate (Go, Swap),
  )
import XMonad.StackSet
import XMonad.Util.DynamicScratchpads
import XMonad.Util.NamedActions

xmonadCmd = "xmonad-x86_64-linux"

data StrutState = SingleWindow | FullScreen | Default

-- match the current strut state, and send the appropriate named action
setStrutState :: IORef StrutState -> X ()
setStrutState ref = do
  state <- io $ readIORef ref
  case state of
    Default -> do
      io $ writeIORef ref SingleWindow
      sendMessage (Toggle FULL)
    SingleWindow -> do
      io $ writeIORef ref FullScreen
      sendMessage ToggleStruts >> sendMessage (Toggle NOBORDERS)
    FullScreen -> do
      io $ writeIORef ref Default
      sendMessage ToggleStruts >> sendMessage (Toggle FULL)
        >> sendMessage
          (Toggle NOBORDERS)

-- NOTE: We are destructuring XConfig below (i.e. Type {property = <local symbol>})
createKeyMaps ::
  String ->
  [String] ->
  IORef StrutState ->
  XConfig Layout ->
  [((KeyMask, KeySym), NamedAction)]
createKeyMaps term werkspaces currentStrutStateRef XConfig {modMask = modm, layoutHook = layoutHk} =
  [ subtitle "Core",
    ( (modm .|. shiftMask, xK_r),
      addName "Restart XMonad" $
        spawn
          ( xmonadCmd
              ++ " --restart && \
                 \ notify-send -a 'XMonad'  'Restarted'"
          )
    ),
    ( (modm .|. controlMask .|. shiftMask, xK_r),
      addName "Recompile and Restart XMonad" $
        spawn
          ( "\
            \ notify-send -a 'XMonad' 'Recompiling...' && \
            \ "
              ++ xmonadCmd
              ++ " --recompile && \
                 \ "
              ++ xmonadCmd
              ++ " --restart && \
                 \ notify-send -a 'XMonad'  'Restarted'"
          )
    ),
    ---
    ---
    ---
    subtitle "Launchers",
    ( (modm .|. shiftMask, xK_Return),
      addName ("Launch " ++ term ++ " terminal") $ spawn term
    ),
    ((modm, xK_p), addName "Launch dmenu" $ spawn "dmenu_run"),
    --
    ((modm, xK_d), addName "Launch rofi" $ spawn "rofi -show drun"),
    --
    ( (modm, xK_s),
      addName "Launch Spell" $ spawn "$HOME/.local/bin/rofi-spell"
    ),
    --
    ( (modm, xK_0),
      addName "Launch power menu" $ spawn "$HOME/.local/bin/powermenu"
    ),
    --
    ((modm, xK_g), addName "Launch grid selector" $ createAppGridSpawner ()),
    --
    ((modm .|. shiftMask, xK_q), addName "close focused window" kill),
    ---
    ---
    ---
    subtitle "Layouts",
    ((modm, xK_Tab), addName "Rotate through Screens" nextScreen),
    --
    ( (modm .|. shiftMask, xK_Tab),
      addName "Rotate through the available layouts" $ sendMessage NextLayout
    ),
    --
    ( (modm .|. shiftMask, xK_space),
      addName "Reset the layouts on the current workspace to default" $
        setLayout layoutHk
    ),
    --
    ( (modm, xK_f),
      addName "Switch between SingleWindow, FullScreen or default" $
        setStrutState currentStrutStateRef
    ),
    ( (modm .|. controlMask, xK_y),
      addName "Flip layout on Y axis" $ sendMessage $ Toggle REFLECTY
    ),
    ( (modm .|. controlMask, xK_x),
      addName "Flip layout on X axis" $ sendMessage $ Toggle REFLECTX
    ),
    ---
    ---
    ---
    subtitle "Focus",
    ( (modm, xK_j),
      addName "Move focus to the next window" $ windows focusDown
    ),
    --
    ( (modm, xK_k),
      addName "Move focus to the previous window" $ windows focusUp
    ),
    --
    ( (modm .|. shiftMask, xK_j),
      addName "Swap the focused window with the next window" $
        windows swapDown
    ),
    --
    ( (modm .|. shiftMask, xK_k),
      addName "Swap the focused window with the previous window" $
        windows swapUp
    ),
    --
    ((modm, xK_h), addName "Shrink the master area" $ sendMessage Shrink),
    --
    ((modm, xK_l), addName "Expand the master area" $ sendMessage Expand),
    --
    ( (modm, xK_t),
      addName "Sink window into layout" $ withFocused $ windows . sink
    ),
    ---
    ---
    ---
    subtitle "Navigation",
    ((modm, xK_Right), addName "Go Right" $ sendMessage $ Go R),
    ((modm, xK_Left), addName "Go Left" $ sendMessage $ Go L),
    ((modm, xK_Up), addName "Go Up" $ sendMessage $ Go U),
    ((modm, xK_Down), addName "Go down" $ sendMessage $ Go D),
    --
    ( (modm .|. shiftMask, xK_Right),
      addName "Swap Right" $ sendMessage $ Swap R
    ),
    ( (modm .|. shiftMask, xK_Left),
      addName "Swap Left" $ sendMessage $ Swap L
    ),
    ((modm .|. shiftMask, xK_Up), addName "Swap Up" $ sendMessage $ Swap U),
    ( (modm .|. shiftMask, xK_Down),
      addName "Swap Down" $ sendMessage $ Swap D
    ),
    ---
    ---
    ---
    subtitle "ScatchPads",
    ((modm, xK_m), addName "Music Scratchpad" $ getAction "music"),
    ( (modm, xK_a),
      addName "Spawn|Toggle Scratchpad" $ spawnDynamicSP "scratch1"
    ),
    ( (modm .|. shiftMask, xK_a),
      addName "Make Window a Scratchpad" $
        withFocused $
          makeDynamicSP
            "scratch1"
    ),
    ---
    ---
    ---
    subtitle "UI",
    ( (modm, xK_z),
      addName "Warp mouse to current screen." $ warpToWindow (1 % 2) (1 % 2)
    ),
    ( (modm, xK_b),
      addName "Banish cursor to corner of screen." $ banishScreen UpperLeft
    ),
    subtitle "Werkspaces"
  ]
    ++
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
    [ ( (keyMask .|. modm, keySym),
        addName (name ++ werkSpace ++ " workspace") $
          windows $
            xAction
              werkSpace
      )
      | (werkSpace, keySym) <- zip werkspaces [xK_1 .. xK_9],
        (xAction, keyMask, name) <-
          [(greedyView, 0, "Switch To "), (shift, shiftMask, "Move To ")]
    ]
