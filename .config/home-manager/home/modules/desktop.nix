{
  pkgs,
  theme,
  nixgl,
  config,
  ...
}: let
  c = theme.colors;
  nixglWrap = config.lib.nixgl.wrapPackage;
  pamShimWrap = config.lib.pamShim.replacePam;
  hyprlock = pamShimWrap (nixglWrap pkgs.hyprlock);
in {
  targets.genericLinux.nixGL.packages = nixgl.packages;

  imports = [
    ./desktop
  ];

  home.packages = with pkgs; [
    dracula-icon-theme # Dracula icon theme
    papirus-icon-theme # Papirus icon theme (dracula does not have nixos icon)
    # nitrogen # wallpaper manager
    # xclip # x11 clipboard cli
    # xplugd # A UNIX daemon that executes a script on X input and RandR changes
    # yad # A fork of Zenity with many improvements
    wev # wayland event viewer
    wl-clipboard-rs # wayland clipboard cli
    mako # wayland notification daemon
  ];

  home.nix-wallpapers = {
    g5 = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.mauve;
        endColor = theme.colors.sapphire;
      };
      colors = {
        color0 = theme.colors.overlay0;
        color1 = theme.colors.overlay0;
        color2 = theme.colors.overlay0;
        color3 = theme.colors.overlay0;
        color4 = theme.colors.overlay0;
        color5 = theme.colors.overlay0;
      };
      height = 1440;
      width = 3440;
    };

    dell = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.lavender;
        endColor = theme.colors.sapphire;
      };
      colors = {
        color0 = theme.colors.subtext1;
        color1 = theme.colors.subtext1;
        color2 = theme.colors.subtext1;
        color3 = theme.colors.subtext1;
        color4 = theme.colors.subtext1;
        color5 = theme.colors.subtext1;
      };
      height = 1080;
      width = 2560;
    };

    framework = {
      angle = 60;
      swirl = 360;
      gradient = {
        beginColor = theme.colors.sapphire;
        endColor = theme.colors.lavender;
      };
      colors = {
        color0 = theme.colors.text;
        color1 = theme.colors.text;
        color2 = theme.colors.text;
        color3 = theme.colors.text;
        color4 = theme.colors.text;
        color5 = theme.colors.text;
      };
      height = 1504;
      width = 2256;
    };
  };

  programs = {
    # requires /etc/pam.d/hyprlock with 644 and 'auth include login'
    hyprlock = {
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
  };
}
