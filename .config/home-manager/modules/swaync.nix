{
  flake.modules.homeManager.swaync = {
    lib,
    config,
    ...
  }: {
    options.swayncOutput = {
      type = lib.types.str;
      default = "";
      description = "Output for swaync";
    };

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
        notification-window-preferred-output = config.swayncOutput;
        notification-body-image-height = 200;
        notification-body-image-width = 200;
      };
    };
  };
}
