{
  inputs,
  lib,
  ...
}: {
  flake.modules.homeManager.powermenu = {
    pkgs,
    config,
    ...
  }: {
    options.programs.powermenu-rs.package = lib.mkOption {
      type = lib.types.package;
      default = config.lib.nixGL.wrap pkgs.powermenu-rs;
      readOnly = true;
      description = "The nixGL-wrapped powermenu-rs package";
    };

    config = {
      nixpkgs.overlays = [
        inputs.powermenu-rs.overlays.default
      ];

      home.packages = [
        # powermenu written in rust with Iced.rs
        # TODO: replace with icedshell
        config.programs.powermenu-rs.package
      ];
    };
  };
}
