{inputs, ...}: {
  flake.modules.homeManager.parinfer = {
    nixpkgs.overlays = [
      inputs.parinfer-rust.overlays.default
      # vimPlugins.parinfer-rust inherits pname from pkgs.parinfer-rust, but
      # the upstream flake builds with `name` (old style) and omits pname.
      (_final: prev: {
        parinfer-rust = prev.parinfer-rust // {pname = "parinfer-rust";};
      })
    ];
  };
}
