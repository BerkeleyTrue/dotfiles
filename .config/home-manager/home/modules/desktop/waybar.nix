{theme, ...}: let
  c = theme.colors;
in {
  catppuccin.waybar.enable = true;
  programs.waybar = {
    enable = true;
    settings = {
      main = {
        layer = "bottom";
        modules-left = ["niri/workspaces"];
        modules-center = ["clock"];
        modules-right = ["battery" "cpu" "memory" "tray"];

        clock.format = " {:%a %b %d | Week %V   %H:%M:%S}";
      };
    };

    style = ''
      window#waybar {
        padding: 0px 10px;
        background: ${c.base};
        color: ${c.text};
      }
    '';
  };
}
