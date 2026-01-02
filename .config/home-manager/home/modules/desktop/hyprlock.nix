{
  pkgs,
  config,
  hardware,
  lib,
  theme,
  ...
}: let
  ca = theme.colors;
  c = theme.colorsRgb;
  nixglWrap = config.lib.nixgl.wrapPackage;
  pamShimWrap = config.lib.pamShim.replacePam;
  hyprlock = pamShimWrap (nixglWrap pkgs.hyprlock);
  backgrounds =
    lib.mapAttrsToList (name: spec: {
      monitor = spec.label;
      path = "${config.home.nix-wallpapers.${name}.outputPath}";
      blur_passes = 1;
      color = c.base;
    })
    hardware.monitors;
in {
  # requires /etc/pam.d/hyprlock with 644 and 'auth include login'
  programs.hyprlock = {
    enable = true;
    package = hyprlock;
    settings = {
      source = ["frappe.conf"];
      # GENERAL
      general = {
        hide_cursor = true;
        immediate_render = true;
      };

      background = backgrounds;

      label = [
        {
          # Layout
          monitor = "";
          text = "Layout: $LAYOUT";
          color = c.text;
          font_size = 25;
          font_family = "$font";
          position = "30, -30";
          halign = "left";
          valign = "top";
        }
        {
          # TIME
          monitor = "";
          text = "$TIME";
          color = c.text;
          font_size = 90;
          font_family = "$font";
          position = "-30, 0";
          halign = "right";
          valign = "top";
        }
        {
          # DATE
          monitor = "";
          text = ''cmd[update:43200000] date +"%A, %d %B %Y"'';
          color = c.text;
          font_size = 25;
          font_family = "$font";
          position = "-30, -150";
          halign = "right";
          valign = "top";
        }
        {
          # FINGERPRINT
          monitor = "";
          text = "$FPRINTPROMPT";
          color = c.text;
          font_size = 14;
          font_family = "$font";
          position = "0, -107";
          halign = "center";
          valign = "center";
        }
      ];

      # USER AVATAR
      image = [
        # {
        #   monitor = "";
        #   path = "$HOME/.face";
        #   size = 100;
        #   border_color = c.mauve;
        #   position = "0, 75";
        #   halign = "center";
        #   valign = "center";
        # }
      ];

      # INPUT FIELD
      input-field = [
        {
          monitor = "";
          size = "300, 60";
          outline_thickness = 4;
          dots_size = 0.2;
          dots_spacing = 0.2;
          dots_center = true;
          outer_color = c.mauve;
          inner_color = c.surface0;
          font_color = c.text;
          fade_on_empty = false;
          placeholder_text = ''<span foreground="#${ca.text}"><i>󰌾 Logged in as </i><span foreground="#${ca.mauve}">$USER</span></span>'';
          hide_input = false;
          check_color = c.mauve;
          fail_color = c.red;
          fail_text = ''<i>$FAIL <b>($ATTEMPTS)</b></i>'';
          capslock_color = c.yellow;
          position = "0, -47";
          halign = "center";
          valign = "center";
        }
      ];
    };
  };
}
