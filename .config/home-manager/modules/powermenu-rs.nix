{inputs, ...}: {
  flake.modules.homeManager.nixgl = {
    nixpkgs.overlays = [
      inputs.powermenu-rs.overlays.default
    ];
  };
}
