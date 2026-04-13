{inputs, ...}: {
  flake.modules.homeManager.nixgl = {
    nixpkgs.overlays = [
      inputs.parinfer-rust.overlays.default
    ];
  };
}
