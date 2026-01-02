{
  profile,
  hardware,
  ...
}: let
  output =
    if profile == "delora"
    then hardware.monitors.g5.label
    else "";
in {
  # catppuccin.swaync.enable = true;
  services.swaync = {
    enable = true;
    settings = {
      font = "FiraCode Nerd Font Mono 10";
      layer = "overlay";
      # set to false to inspect control panel without it closing
      layer-shell = true;
      cssPriority = "user";
      notification-window-width = 350;
      notification-window-preferred-output = output;
      notification-body-image-height = 200;
      notification-body-image-width = 200;
    };
  };
}
