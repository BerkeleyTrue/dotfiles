{inputs, ...}: {
  flake.modules.homeManager.powermenu = {
    pkgs,
    config,
    ...
  }: {
    nixpkgs.overlays = [
      inputs.powermenu-rs.overlays.default
    ];

    home.packages = [
      # powermenu written in rust with Iced.rs
      # TODO: replace with icedshell
      (config.lib.nixGL.wrap pkgs.powermenu-rs)
    ];
  };
}
