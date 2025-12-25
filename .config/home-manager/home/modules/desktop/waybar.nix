{...}: {
  catppuccin.waybar.enable = true;
  programs.waybar = {
    enable = true;
    settings = {
      main = {
        layer = "bottom";
        height = 22;
        modules-left = ["clock" "niri/workspaces"];
        modules-center = ["niri/window"];
        modules-right = ["pulseaudio" "network" "cpu" "memory"];
      };
    };
  };
}
