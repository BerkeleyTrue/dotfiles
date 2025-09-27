{
  pkgs,
  theme,
  lib,
  config,
  ...
}: let
  toDunstIni = lib.generators.toINI {
    mkKeyValue = key: value: let
      value' =
        if lib.isBool value
        then (lib.hm.booleans.yesNo value)
        else if lib.isString value
        then ''"${value}"''
        else toString value;
    in "${key}=${value'}";
  };
  c = theme.colors;

  settings = {
    global = {
      frame_width = 1;
      frame_color = c.rosewater;
      font = "FiraCode Nerd Font Mono 10";
      format = "<b>%a</b>: <i>%s</i> %p\\n%b";
      markup = "full";
      sort = "no";
      indicate_hidden = "yes";
      alignment = "left";
      show_age_threshold = 60;
      word_wrap = "yes";
      ellipsize = "end";
      corner_radius = 8;
      mouse_left_click = "close_current";
      mouse_right_click = "do_action";
      ignore_newline = "no";
      width = "(200, 400)";
      height = 400;
      offset = "10x30";
      shrink = "yes";
      transparency = 15;
      idle_threshold = 60;
      monitor = 0;
      follow = "none";
      sticky_history = true;
      history_length = 100;

      # Display indicators for URLs (U) and actions (A).
      show_indicators = false;

      # The height of a single line.  If the height is smaller than the
      # font height, it will get raised to the font height.
      # This adds empty space above and under the text.
      line_height = 1;

      # Draw a line of "separator_height" pixel height between two
      # notifications.
      # Set to 0 to disable.
      separator_height = 1;

      # Padding between text and separator.
      # padding = 8
      padding = 8;

      # Horizontal padding.
      horizontal_padding = 10;

      # Define a color for the separator.
      # possible values are:
      #  * auto: dunst tries to find a color fitting to the background;
      #  * foreground: use the same color as the foreground;
      #  * frame: use the same color as the frame;
      #  * anything else will be interpreted as a X color.
      separator_color = c.lavender;

      # dmenu path.
      dmenu = "${pkgs.rofi} -dmenu -p dunst:";

      # Browser for opening urls in context menu.
      browser = "org.mozilla.firefox";

      # Align icons left/right/off
      icon_position = "left";

      # Paths to default icons.

      # Limit icons size.
      max_icon_size = 100;

      # uses XDG icon lookup
      # requires xdg_data_dirs to be set up correctly for systemd.
      # user bus is not guaranteed to have this set up correctly.
      # see xdg.configFile."systemd/user.conf"
      enable_recursive_icon_lookup = true;
      icon_theme = "Dracula,Papirus-Dark,hicolor";
    };

    urgency_low = {
      background = c.surface1;
      foreground = c.subtext1;
      timeout = 10;
    };

    urgency_normal = {
      background = c.base;
      foreground = c.text;
      timeout = 10;
    };

    urgency_critical = {
      background = c.red;
      foreground = c.surface1;
      frame_color = c.rosewater;
      timeout = 0;
    };
  };
in {
  home.packages = [pkgs.dunst];

  xdg.dataFile."dbus-1/services/org.knopwob.dunst.service".source = "${pkgs.dunst}/share/dbus-1/services/org.knopwob.dunst.service";

  # custom settings for icon_path fix
  xdg.configFile."dunst/dunstrc" = {
    text = toDunstIni settings;
    onChange = ''
      ${pkgs.procps}/bin/pkill -u "$USER" ''${VERBOSE+-e} dunst || true
    '';
  };

  systemd.user.services.dunst = {
    Unit = {
      Description = "Dunst notification daemon";
      Documentation = ["man:dunst(1)"];
      After = ["compositor.target"];
      PartOf = ["notification.target"];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.dunst}/bin/dunst -config ${config.xdg.configHome}/dunst/dunstrc";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["notification.target"];
    };
  };
}
