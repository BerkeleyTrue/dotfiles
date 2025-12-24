{
  kdl,
  profile,
  lib,
  hardware,
  theme,
  ...
}: let
  c = theme.colors;
  inherit (kdl) plain leaf flag;
  output =
    if profile == "delora"
    then
      import ./outputs/delora.nix {
        inherit hardware theme;
        inherit (kdl) node plain leaf flag;
      }
    else
      import ./outputs/rena.nix {
        inherit hardware theme;
        inherit (kdl) node plain leaf flag;
      };
in
  [
    (plain "input" [
      (plain "keyboard" [
        (plain "xkb" [
          # man xkeyboard-config(7).
          (leaf "layout" "us")
        ])
      ])

      # Next sections include libinput settings.
      # Omitting settings disables them, or leaves them at their default values.
      (plain "touchpad" [
        (flag "tap")
        (flag "natural-scroll")
      ])

      (plain "mouse" [])

      # Don't take over power key
      (flag "disable-power-key-handling")
      (flag "focus-follows-mouse")
    ])

    (plain "layout" [
      (plain "focus-ring" [
        (flag "off")
        (leaf "width" 4)
        (leaf "inactive-color" c.base)
        (leaf "active-gradient" {
          from = c.rosewater;
          to = c.mauve;
          angle = 33;
          relative-to = "workspace-view";
        })
      ])

      # You can also add a border. It's similar to the focus ring, but always visible.
      (plain "border" [
        # The settings are the same as for the focus ring.
        # If you enable the border, you probably want to disable the focus ring.
        # (flag "off")

        (leaf "width" 4)
        (leaf "inactive-gradient" {
          from = c.surface0;
          to = c.surface2;
          angle = 33;
          relative-to = "workspace-view";
        })
        (leaf "urgent-color" c.red)
        (leaf "active-gradient" {
          from = c.rosewater;
          to = c.mauve;
          angle = 33;
          relative-to = "workspace-view";
        })
      ])

      (plain "shadow" [
        (flag "off")
      ])

      # (plain "preset-column-widths" [])
      (plain "default-column-width" [])

      (leaf "gaps" 6)
      (plain "struts" [
        (leaf "left" 8)
        (leaf "right" 14)
        (leaf "top" 4)
        (leaf "bottom" 4)
      ])

      (leaf "center-focused-column" "on-overflow")
      (flag "always-center-single-column")
    ])

    # Add lines like this to spawn processes at startup.
    # Note that running niri as a session supports xdg-desktop-autostart,
    # which may be more convenient to use.
    # (leaf "spawn-at-startup" [ "alacritty" "-e" "fish" ])

    # You can override environment variables for processes spawned by niri.
    (plain "environment" [
      # unset x11 variables
      (leaf "DISPLAY" null)
    ])

    (plain "cursor" [
      # Change the theme and size of the cursor as well as set the
      # `XCURSOR_THEME` and `XCURSOR_SIZE` env variables.
      # (leaf "xcursor-theme" "default")
      # (leaf "xcursor-size" 24)
    ])

    # Uncomment this line to ask the clients to omit their client-side decorations if possible.
    # If the client will specifically ask for CSD, the request will be honored.
    # Additionally, clients will be informed that they are tiled, removing some rounded corners.
    (flag "prefer-no-csd")

    # You can change the path where screenshots are saved.
    # A ~ at the front will be expanded to the home directory.
    # The path is formatted with strftime(3) to give you the screenshot date and time.
    (leaf "screenshot-path" "~/pics/screenshots/Screenshot_from_%Y-%m-%d %H-%M-%S.png")

    # You can also set this to null to disable saving screenshots to disk.
    # (leaf "screenshot-path" null)

    # Settings for the "Important Hotkeys" overlay.
    (plain "hotkey-overlay" [
      # Uncomment this line if you don't want to see the hotkey help at niri startup.
      (flag "skip-at-startup")
    ])

    # Animation settings.
    (plain "animations" [
      # Uncomment to turn off all animations.
      # (flag "off")

      # Slow down all animations by this factor. Values below 1 speed them up instead.
      # (leaf "slowdown" 3.0)

      # You can configure all individual animations.
      # Available settings are the same for all of them.
      # - off disables the animation.
      #
      # Niri supports two animation types: easing and spring.
      # You can set properties for only ONE of them.
      #
      # Easing has the following settings:
      # - duration-ms sets the duration of the animation in milliseconds.
      # - curve sets the easing curve. Currently, available curves
      #   are "ease-out-cubic" and "ease-out-expo".
      #
      # Spring animations work better with touchpad gestures, because they
      # take into account the velocity of your fingers as you release the swipe.
      # The parameters are less obvious and generally should be tuned
      # with trial and error. Notably, you cannot directly set the duration.
      # You can use this app to help visualize how the spring parameters
      # change the animation: https://flathub.org/apps/app.drey.Elastic
      #
      # A spring animation is configured like this:
      # - (leaf "spring" { damping-ratio=1.0; stiffness=1000; epsilon=0.0001; })
      #
      # The damping ratio goes from 0.1 to 10.0 and has the following properties:
      # - below 1.0: underdamped spring, will oscillate in the end.
      # - above 1.0: overdamped spring, won't oscillate.
      # - 1.0: critically damped spring, comes to rest in minimum possible time
      #    without oscillations.
      #
      # However, even with damping ratio = 1.0 the spring animation may oscillate
      # if "launched" with enough velocity from a touchpad swipe.
      #
      # Lower stiffness will result in a slower animation more prone to oscillation.
      #
      # Set epsilon to a lower value if the animation "jumps" in the end.
      #
      # The spring mass is hardcoded to 1.0 and cannot be changed. Instead, change
      # stiffness proportionally. E.g. increasing mass by 2x is the same as
      # decreasing stiffness by 2x.

      # Animation when switching workspaces up and down,
      # including after the touchpad gesture.
      (plain "workspace-switch" [
        # (flag "off")
        # (leaf "spring" { damping-ratio=1.0; stiffness=1000; epsilon=0.0001; })
      ])

      # All horizontal camera view movement:
      # - When a window off-screen is focused and the camera scrolls to it.
      # - When a new window appears off-screen and the camera scrolls to it.
      # - When a window resizes bigger and the camera scrolls to show it in full.
      # - And so on.
      (plain "horizontal-view-movement" [
        # (flag "off")
        # (leaf "spring" { damping-ratio=1.0; stiffness=800; epsilon=0.0001; })
      ])

      # Window opening animation. Note that this one has different defaults.
      (plain "window-open" [
        # (flag "off")
        # (leaf "duration-ms" 150)
        # (leaf "curve" "ease-out-expo")

        # Example for a slightly bouncy window opening:
        # (leaf "spring" { damping-ratio=0.8; stiffness=1000; epsilon=0.0001; })
      ])

      # Config parse error and new default config creation notification
      # open/close animation.
      (plain "config-notification-open-close" [
        # (flag "off")
        # (leaf "spring" { damping-ratio=0.6; stiffness=1000; epsilon=0.001; })
      ])
    ])

    (plain "window-rule" [
      (leaf "geometry-corner-radius" 8)
      (leaf "clip-to-geometry" true)
    ])

    (plain "window-rule" [
      (leaf "match" {
        is-focused = false;
      })
      (leaf "opacity" 0.9)
    ])

    (plain "binds" [
      # Mod-? shows a list of important hotkeys.
      (plain "Mod+Shift+Slash" [(flag "show-hotkey-overlay")])

      (plain "Mod+Shift+Return" [(leaf "spawn" ["kitty"])])
      (plain "Mod+D" [(leaf "spawn" ["anyrun"])])
      # (plain "Super+Alt+L" [(leaf "spawn" ["swaylock"])])

      (plain "Mod+Shift+Q" [(flag "close-window")])

      (plain "Mod+Left" [(flag "focus-column-left")])
      (plain "Mod+Down" [(flag "focus-window-down")])
      (plain "Mod+Up" [(flag "focus-window-up")])
      (plain "Mod+Right" [(flag "focus-column-right")])
      (plain "Mod+H" [(flag "focus-column-left")])
      (plain "Mod+J" [(flag "focus-window-down")])
      (plain "Mod+K" [(flag "focus-window-up")])
      (plain "Mod+L" [(flag "focus-column-right")])

      (plain "Mod+Shift+Left" [(flag "move-column-left")])
      (plain "Mod+Shift+Down" [(flag "move-window-down")])
      (plain "Mod+Shift+Up" [(flag "move-window-up")])
      (plain "Mod+Shift+Right" [(flag "move-column-right")])
      (plain "Mod+Shift+H" [(flag "move-column-left")])
      (plain "Mod+Shift+J" [(flag "move-window-down")])
      (plain "Mod+Shift+K" [(flag "move-window-up")])
      (plain "Mod+Shift+L" [(flag "move-column-right")])

      (plain "Mod+Home" [(flag "focus-column-first")])
      (plain "Mod+End" [(flag "focus-column-last")])
      (plain "Mod+Ctrl+Home" [(flag "move-column-to-first")])
      (plain "Mod+Ctrl+End" [(flag "move-column-to-last")])

      # (plain "Mod+Shift+Left" [(flag "focus-monitor-left")])
      # (plain "Mod+Shift+Down" [(flag "focus-monitor-down")])
      # (plain "Mod+Shift+Up" [(flag "focus-monitor-up")])
      # (plain "Mod+Shift+Right" [(flag "focus-monitor-right")])
      # (plain "Mod+Shift+H" [(flag "focus-monitor-left")])
      # (plain "Mod+Shift+J" [(flag "focus-monitor-down")])
      # (plain "Mod+Shift+K" [(flag "focus-monitor-up")])
      # (plain "Mod+Shift+L" [(flag "focus-monitor-right")])

      (plain "Mod+Shift+Ctrl+Left" [(flag "move-column-to-monitor-left")])
      (plain "Mod+Shift+Ctrl+Down" [(flag "move-column-to-monitor-down")])
      (plain "Mod+Shift+Ctrl+Up" [(flag "move-column-to-monitor-up")])
      (plain "Mod+Shift+Ctrl+Right" [(flag "move-column-to-monitor-right")])
      (plain "Mod+Shift+Ctrl+H" [(flag "move-column-to-monitor-left")])
      (plain "Mod+Shift+Ctrl+J" [(flag "move-column-to-monitor-down")])
      (plain "Mod+Shift+Ctrl+K" [(flag "move-column-to-monitor-up")])
      (plain "Mod+Shift+Ctrl+L" [(flag "move-column-to-monitor-right")])

      (plain "Mod+Page_Down" [(flag "focus-workspace-down")])
      (plain "Mod+Page_Up" [(flag "focus-workspace-up")])
      (plain "Mod+U" [(flag "focus-workspace-down")])
      (plain "Mod+I" [(flag "focus-workspace-up")])
      (plain "Mod+Ctrl+Page_Down" [(flag "move-column-to-workspace-down")])
      (plain "Mod+Ctrl+Page_Up" [(flag "move-column-to-workspace-up")])
      (plain "Mod+Ctrl+U" [(flag "move-column-to-workspace-down")])
      (plain "Mod+Ctrl+I" [(flag "move-column-to-workspace-up")])

      (plain "Mod+Shift+Page_Down" [(flag "move-workspace-down")])
      (plain "Mod+Shift+Page_Up" [(flag "move-workspace-up")])
      (plain "Mod+Shift+U" [(flag "move-workspace-down")])
      (plain "Mod+Shift+I" [(flag "move-workspace-up")])

      (plain "Mod+1" [(leaf "focus-workspace" 1)])
      (plain "Mod+2" [(leaf "focus-workspace" 2)])
      (plain "Mod+3" [(leaf "focus-workspace" 3)])
      (plain "Mod+4" [(leaf "focus-workspace" 4)])
      (plain "Mod+5" [(leaf "focus-workspace" 5)])
      (plain "Mod+6" [(leaf "focus-workspace" 6)])
      (plain "Mod+7" [(leaf "focus-workspace" 7)])
      (plain "Mod+8" [(leaf "focus-workspace" 8)])
      (plain "Mod+9" [(leaf "focus-workspace" 9)])
      (plain "Mod+Ctrl+1" [(leaf "move-column-to-workspace" 1)])
      (plain "Mod+Ctrl+2" [(leaf "move-column-to-workspace" 2)])
      (plain "Mod+Ctrl+3" [(leaf "move-column-to-workspace" 3)])
      (plain "Mod+Ctrl+4" [(leaf "move-column-to-workspace" 4)])
      (plain "Mod+Ctrl+5" [(leaf "move-column-to-workspace" 5)])
      (plain "Mod+Ctrl+6" [(leaf "move-column-to-workspace" 6)])
      (plain "Mod+Ctrl+7" [(leaf "move-column-to-workspace" 7)])
      (plain "Mod+Ctrl+8" [(leaf "move-column-to-workspace" 8)])
      (plain "Mod+Ctrl+9" [(leaf "move-column-to-workspace" 9)])

      (plain "Mod+Comma" [(flag "consume-window-into-column")])
      (plain "Mod+Period" [(flag "expel-window-from-column")])

      (plain "Mod+R" [(flag "switch-preset-column-width")])
      (plain "Mod+F" [(flag "maximize-column")])
      (plain "Mod+Shift+F" [(flag "fullscreen-window")])
      (plain "Mod+C" [(flag "center-column")])

      # Finer width adjustments.
      # This command can also:
      # * set width in pixels: "1000"
      # * adjust width in pixels: "-5" or "+5"
      # * set width as a percentage of screen width: "25%"
      # * adjust width as a percentage of screen width: "-10%" or "+10%"
      # Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
      # (leaf "set-column-width" "100") will make the column occupy 200 physical screen pixels.
      (plain "Mod+Minus" [(leaf "set-column-width" "-10%")])
      (plain "Mod+Equal" [(leaf "set-column-width" "+10%")])

      # Finer height adjustments when in column with other windows.
      (plain "Mod+Shift+Minus" [(leaf "set-window-height" "-10%")])
      (plain "Mod+Shift+Equal" [(leaf "set-window-height" "+10%")])

      # Actions to switch layouts.
      # Note: if you uncomment these, make sure you do NOT have
      # a matching layout switch hotkey configured in xkb options above.
      # Having both at once on the same hotkey will break the switching,
      # since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
      # (plain "Mod+Space"       [(leaf "switch-layout" "next")])
      # (plain "Mod+Shift+Space" [(leaf "switch-layout" "prev")])

      # This debug bind will tint all surfaces green, unless they are being
      # directly scanned out. It's therefore useful to check if direct scanout
      # is working.
      # (plain "Mod+Shift+Ctrl+T" [(flag "toggle-debug-tint")])
    ])

    # Settings for debugging. Not meant for normal use.
    # These can change or stop working at any point with little notice.
    (plain "debug" [
      # Make niri take over its DBus services even if it's not running as a session.
      # Useful for testing screen recording changes without having to relogin.
      # The main niri instance will *not* currently take back the services; so you will
      # need to relogin in the end.
      # (flag "dbus-interfaces-in-non-session-instances")

      # Wait until every frame is done rendering before handing it over to DRM.
      # (flag "wait-for-frame-completion-before-queueing")

      # Enable direct scanout into overlay planes.
      # May cause frame drops during some animations on some hardware.
      # (flag "enable-overlay-planes")

      # Disable the use of the cursor plane.
      # The cursor will be rendered together with the rest of the frame.
      # (flag "disable-cursor-plane")

      # Override the DRM device that niri will use for all rendering.
      # (leaf "render-drm-device" "/dev/dri/renderD129")

      # Enable the color-transformations capability of the Smithay renderer.
      # May cause a slight decrease in rendering performance.
      # (flag "enable-color-transformations-capability")

      # Emulate zero (unknown) presentation time returned from DRM.
      # This is a thing on NVIDIA proprietary drivers, so this flag can be
      # used to test that we don't break too hard on those systems.
      # (flag "emulate-zero-presentation-time")
    ])
  ]
  ++ output
# source: https://github.com/sodiboo/niri-flake/blob/main/default-config.kdl.nix
# This config is structured as KDL.
# This means, that the document is a tree of nodes.
#
# At the top of the config is a list of nodes. Each node has:
# - exactly one name, which is a string
# - zero or more ordered arguments, which are scalars
# - zero or more unordered properties, which are attrsets of scalars
# - zero or more ordered children, which are nodes
#
# KDL also permits "type names", but niri does not use them.
#
# Scalars are strings, integers, floats, booleans, or null.
#
# This default config uses the kdl library from https://github.com/sodiboo/niri-flake/blob/main/kdl.nix.
# It provides a set of functions to declare KDL documents.
#
# The fundamental function is `kdl.node`, which takes a name, a list of arguments, and a list of children.
# To declare properties, you should pass an attrset to `kdl.node` as the last argument.
#
# For your convenience, the following transformations are applied to the given parameters:
#
# - The arguments, if not a list, are wrapped in a list.
#   - This means that you can pass a single argument without wrapping it in a list.
#   - You can also pass properties directly, if there are no arguments.
#
# - The children are flattened, and any nulls are removed.
#   - This means that you can call functions directly in the children list.
#   - You can also create "conditional" nodes by setting them to null.
#   - You can see this feature used to the fullest at https://github.com/sodiboo/niri-flake/blob/main/settings.nix
#   - This transformation is also applied at the top-level.
#
# Additionally, kdl.nix provides some shorthand functions to omit the arguments list, children list, or both:
# - `kdl.plain` for nodes with no arguments
# - `kdl.leaf` for nodes with no children
# - `kdl.flag` for nodes with no arguments or children
#
# With that out of the way, here's the nixfied default config for niri.

