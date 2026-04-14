{inputs, ...}: {
  flake.modules.homeManager.parinfer = {
    nixpkgs.overlays = [
      inputs.parinfer-rust.overlays.default
    ];
  };
}
