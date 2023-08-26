{ pkgs, theme, ... }:
{
  home.packages = [ pkgs.dunst ];
  services.dunst = with theme.colors; {
    enable = true;

    iconTheme = {
      name = "Dracula Theme";
      package = pkgs.dracula-icon-theme;
    };

    settings = {
      global = {
        frame_width = 1;
        frame_color = rosewater;
        font = "FiraCode Nerd Font 10";
        format = "<b>%a</b>: <i>%s</i> %p\n%b";
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
        separator_color = lavender;

        # dmenu path.
        dmenu = "${pkgs.rofi} -dmenu -p dunst:";

        # Browser for opening urls in context menu.
        browser = "org.mozilla.firefox";


        # Align icons left/right/off
        icon_position = "left";

        # Paths to default icons.
        # icon_path = /usr/share/icons/Papirus/48x48/devices/:/usr/share/icons/Papirus/48x48/status/:/usr/share/icons/Papirus/48x48/apps/

        # Limit icons size.
        max_icon_size = 100;
      };

      urgency_low = {
        background = surface1;
        foreground = subtext1;
        timeout = 10;
      };

      urgency_normal = {
        background = base;
        foreground = text;
        timeout = 10;
      };

      urgency_critical = {
        background = red;
        foreground = surface1;
        frame_color = rosewater;
        timeout = 0;
      };
    };
  };
}
