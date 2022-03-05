module Berks.KeyMaps
  ( createKeyMaps
  ) where

import Data.Ratio as Ratio ((%))

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W

import qualified XMonad.Actions.Warp as Warp
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))

import XMonad.Util.NamedActions

-- layout modifiers
import XMonad.Layout.MultiToggle as Mt (Toggle(Toggle))
import XMonad.Layout.Reflect as LR
import XMonad.Layout.WindowNavigation as WN
  ( Direction2D(D, L, R, U)
  , Navigate(Go, Swap)
  )

import qualified Berks.GridSelect as GS
import qualified XMonad.Actions.CycleWS as CWs

-- NOTE: We are deconstructuring XConfig below (i.e. Type {property = <local symbol>})
createKeyMaps ::
     String -> [String] -> XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
createKeyMaps term werkspaces XConfig {modMask = modm, layoutHook = layoutHk} =
  [ subtitle "Core"
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
  ---
  ---
  ---
  , subtitle "Launchers"
  , ( (modm .|. shiftMask, xK_Return)
    , addName ("Launch " ++ term ++ " terminal") $ spawn term)
  , ((modm, xK_p), addName "Launch dmenu" $ spawn "dmenu_run")
  --
  , ((modm, xK_d), addName "Launch rofi" $ spawn "rofi -show drun")
  --
  , ((modm, xK_s), addName "Launch Spell" $ spawn "$HOME/.local/bin/rofi-spell")
  --
  , ( (modm, xK_0)
    , addName "Launch power menu" $ spawn "$HOME/.local/bin/powermenu")
  --
  , ((modm, xK_g), addName "Launch grid selector" $ GS.createAppGridSpawner ())
  --
  , ((modm .|. shiftMask, xK_q), addName "close focused window" kill)
  ---
  ---
  ---
  , subtitle "Layouts"
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
  ---
  ---
  ---
  , subtitle "Focus"
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
  ---
  ---
  ---
  , subtitle "Navigation"
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
  ---
  ---
  ---
  , subtitle "UI"
  , ( (modm, xK_z)
    , addName "Warp mouse to current screeen." $
      Warp.warpToWindow (1 % 2) (1 % 2))
  , ( (modm, xK_b)
    , addName "Banish cursor to corner of screen." $
      Warp.banishScreen Warp.UpperLeft)
  , subtitle "Werkspaces"
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
