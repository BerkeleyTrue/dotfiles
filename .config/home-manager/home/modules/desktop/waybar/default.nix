{
  lib,
  profile,
  pkgs,
  theme,
  hardware,
  config,
  ...
}: let
  getExe = lib.getExe;
  wttrbar = pkgs.wttrbar;
  c = theme.colors;
  common-modules = {
    "clock#date" = {
      format = '' {:%a %b %d <span fgcolor="${c.blue}">|</span> Week %V}'';
      interval = 60;
    };

    clock = {
      format = "  {:%H:%M:%S} ";
      interval = 1;
    };

    tray.spacing = 4;

    cpu = {
      interval = 1;
      format = ''<span foreground="${c.pink}"> </span>{usage:2}%'';
      states = {
        danger = 90;
        warning = 75;
      };
    };

    memory = {
      interval = 1;
      format = ''<span foreground="${c.flamingo}"> </span>{percentage}%'';
      states = {
        danger = 90;
        warning = 75;
      };
    };

    disk = {
      interval = 60;
      # use letter spacing here to get some distance between icon and number
      format = ''<span foreground="${c.lavender}">󰉉</span> {specific_free:.0f}G'';
      unit = "GB";
    };

    pulseaudio = {
      format = ''<span foreground="${c.pink}">{icon}</span>  {volume}%'';
      format-bluetooth = ''<span foreground="${c.pink}">{icon}</span>  {volume}%'';
      format-muted = "󰝟";
      format-icons = {
          headphone = "";
          hands-free = "󱥋";
          headset = "󰋎";
          phone = "";
          phone-muted = "";
          portable = "󰥰";
          car = "";
          default = ["" ""];
      };
      scroll-step = 1;
      on-click = "pavucontrol";
    };

    "niri/workspaces" = {
      format = "{icon}";
      format-icons = {
        "active" = ''<span foreground="${c.maroon}"></span>'';
        "urgent" = "󰧞";
        "default" = "󰧞";
      };
    };

    "niri/window" = {
      format = "{}";
      max-length = 15;
      rewrite = {
        "(.*) - zsh" = "> [$1]";
      };
      separate-outputs = true;
    };

    "custom/separator" = {
      format = "|";
    };

    "custom/wakatime" = {
      interval = 10;
      format = " {text}";
      exec = "wakatoday";
    };

    "custom/wttr" = {
      interval = 1200;
      tooltip = true;
      return-type = "json";
      format = "{}°";
      exec = "${getExe wttrbar} --location 'pittsburg california' --fahrenheit";
    };

    "custom/eth" = {
      interval = 10;
      exec = "crypto-egg-go price eth";
      format = ''<span foreground="${c.teal}"></span> {text}'';
    };

    "custom/btc" = {
      interval = 10;
      exec = "crypto-egg-go price btc";
      format = ''<span foreground="${c.peach}"></span> {text}'';
    };

    "custom/connectivity" = {
      "restart-interval" = 1;
      # ping cloudflare DNS to check for connectivity
      exec = "connectivity";
      format = "{}";
    };

    "custom/powermenu" = {
      interval = 0;
      on-click = "${config.lib.nixgl.wrapPackage pkgs.powermenu-rs}/bin/powermenu-rs";
      format = "  ";
    };

    "custom/cpu-temp" = {
      interval = 5;
      # convert temp to json
      exec = "multicoretemp";
      return-type = "json";
      format = "{text}";
    };

    "custom/swaync" = {
      tooltip = true;
      format = "{icon} ";
      format-icons = {
        notification = ''<span foreground="${c.sky}">󱅫</span>'';
        none = ''<span foreground="${c.subtext1}">󰂜</span>'';
        dnd-notification = ''<span foreground="${c.red}">󰂠</span>'';
        dnd-none = ''<span foreground="${c.maroon}">󰪓</span>'';
        inhibited-notification = ''<span foreground="${c.red}">󰂛</span>'';
        inhibited-none = ''<span foreground="${c.maroon}">󰪑</span>'';
        dnd-inhibited-notification = ''<span foreground="${c.red}">󰂛</span>'';
        dnd-inhibited-none = ''<span foreground="${c.maroon}">󰪑</span>'';
      };
      return-type = "json";
      exec-if = "which swaync-client";
      exec = "swaync-client -swb";
      on-click = "swaync-client -t -sw";
      on-click-right = "swaync-client -d -sw";
      escape = true;
    };
  };
  bars = import ./profiles.nix {
    inherit lib profile common-modules hardware;
  };
in {
  home.packages = [wttrbar];

  programs.waybar = {
    enable = true;
    settings = bars;

    # use style file directly for faster iteration
    style = null;
  };
}
