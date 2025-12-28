# niri currently doesn't impl hyprland lock notify v1 api, so on_lock_cmd and on_unlock_cmd won't work
# see https://github.com/YaLTeR/niri/issues/2573
{
  pkgs,
  lib,
  config,
  theme,
  ...
}: let
  nixglWrap = config.lib.nixgl.wrapPackage;
  pamShimWrap = config.lib.pamShim.replacePam;
  getExe = lib.getExe;
  makoctl = getExe pkgs.mako;
  playerctl = getExe pkgs.playerctl;
  hyprlock = pamShimWrap (nixglWrap pkgs.hyprlock);
  niri = getExe pkgs.niri;
  thirty_min = 1800;

  c = theme.colors;
in {
  # NOTE: set up /etc/systemd/logind.conf to lock on lid close
  services.hypridle = {
    enable = true;
    package = config.lib.nixgl.wrapPackage pkgs.hypridle;
    settings = {
      general = {
        before_sleep_cmd = "loginctl lock-session";
        after_sleep_cmd = "${niri} msg action power-on-monitors";

        # lock script handles before and after lock logic
        # triggers on loginctl lock-session
        # don't lock if hyprlock is already running
        lock_cmd = "pidof hyprlock || ~/.local/bin/lock";

        # won't work until niri implements hyprland lock notify v1 api
        # see lock script in local bin
        on_lock_cmd = "${playerctl} pause; ${makoctl} mode -a dnd";
        on_unlock_cmd = "${makoctl} mode -r dnd; makoify -a 'Hephaestus' -u low -i distributor-logo-nixos 'Welcome Back!'";
      };
      listener = [
        {
          timeout = thirty_min; #
          on-timeout = "brightnessctl set 10%";
          on-resume = "brightnessctl -r";
        }
        {
          timeout = thirty_min + 330; # 5.5min
          on-timeout = "niri msg action power-off-monitors";
          on-resume = "niri msg action power-on-monitors && brightnessctl -r";
        }
        {
          # suspend after 10 min of lid closed
          timeout = 600; # 10 min
          on-timeout = "grep -q closed /proc/acpi/button/lid/LID0/state && systemctl suspend";
        }
      ];
    };
  };

  catppuccin.hyprlock.enable = true;
  # requires /etc/pam.d/hyprlock with 644 and 'auth include login'
  programs.hyprlock = {
    enable = true;
    package = hyprlock;
    settings = {
      # GENERAL
      general = {
        hide_cursor = true;
        immediate_render = true;
      };

      label = {
        text = "Unlock?";
        text_align = "center";
        color = c.text;
        font_size = 30;
        # font_family = "Kaushan Script";

        position = "0, 80";
        halign = "center";
        valign = "center";
      };

      # INPUT FIELD
      input-field = {
        # monitor =;
        size = "200, 50";
        outline_thickness = -1;
        dots_size = 0.2; # Scale of input-field height, 0.2 - 0.8
        dots_spacing = 0.2; # Scale of dots' absolute size, 0.0 - 1.0
        dots_center = false;
        outer_color = c.lavender;
        inner_color = c.text;
        font_color = c.base;
        fade_on_empty = false;
        # placeholder_text =;
        hide_input = false;
        position = "800, -15";
        halign = "left";
        valign = "center";
      };
    };
  };

  # these are added here for posterity, but have to be set in /etc/acpi/ following https://wiki.archlinux.org/title/acpid
  xdg.configFile."acpi/events/lidconf".text = ''
    event=button/lid
    action=/etc/acpi/actions/lid.sh "%e"
  '';

  # this will get lid events from root into user land through dbus
  # currently unused
  xdg.configFile."acpi/actions/lid.sh".text = ''
    #!/bin/bash
    state=$(echo "$1" | cut -d " " -f 3)
    case "$state" in
    open)
      # send dbus systemd signal
      # currently unused
      # used this in x11 to trigger mouse movement to unlock screen
      dbus-send \
        --system \
        --type=signal \
        /org/freedesktop/Laptop \
        org.freedesktop.LaptopInterface.LidIsOpen
      ;;
    close)
      # do nothing
      ;;
    *)
      # panic: not a state I know about!
      echo "PANIC STATE" >>/var/logs/lidaction.log
      ;;
    esac
  '';
}
