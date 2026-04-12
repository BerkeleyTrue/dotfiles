{inputs, ...}: {
  flake.modules.homeManager.nixgl = {
    nixpkgs.overlays = [
      inputs.nixgl.overlay
    ];
  };
}
