{
  lib,
  profile,
  pkgs,
  theme,
  hardware,
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
      format = " {:%H:%M:%S}";
      interval = 1;
    };

    tray.spacing = 4;

    cpu = {
      interval = 1;
      format = " {usage}%";
      states = {
        danger = 90;
        warning = 75;
      };
    };

    memory = {
      interval = 1;
      format = " {percentage}%";
      states = {
        danger = 90;
        warning = 75;
      };
    };

    disk = {
      interval = 60;
      # use letter spacing here to get some distance between icon and number
      format = "󰉉 {specific_free:.0f}G";
      unit = "GB";
    };

    "niri/workspaces" = {
      format = "{icon}";
      format-icons = {
        "1" = "󰇊";
        "2" = "󰇋";
        "3" = "󰇌";
        "4" = "󰇍";
        "5" = "󰇎";
        "6" = "󰇏";
      };
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
      format = ''<span class="icon eth"></span> {text}'';
    };

    "custom/btc" = {
      interval = 10;
      exec = "crypto-egg-go price btc";
      format = ''<span class="icon btc"></span> {text}'';
    };

    "custom/connectivity" = {
      "restart-interval" = 1;
      # ping cloudflare DNS to check for connectivity
      exec = "connectivity";
      format = "{}";
    };

    "custom/powermenu" = {
      interval = 0;
      on-click = "powermenu";
      format = "  ";
    };

    "custom/cpu-temp" = {
      interval = 5;
      # convert temp to json
      exec = "multicoretemp | jq --unbuffered --compact-output '{percentage: .}'";
      return-type = "json";
      format = "{icon} {percentage}°C";
      format-icons = [
        ''<span fgcolor="${c.sapphire}"></span>''
        ''<span fgcolor="${c.yellow}"></span>''
        ''<span fgcolor="${c.red}"></span>''
      ];
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
