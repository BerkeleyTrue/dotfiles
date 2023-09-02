{
  description = "My special snow flake";

  inputs = {
    nixpkgs.url = "github:nixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR"; # nix user packages
    };

    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };

    parinfer-rust = {
      url = "github:PhilTaken/parinfer-rust";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, flake-parts, parinfer-rust, ... }:
    flake-parts.lib.mkFlake { inherit self inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ ./home-manager.nix ];
      perSystem = { system, ... }:
        with inputs;
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              nixgl.overlay
              (final: prev: {
                rofi-network-manager = prev.pkgs.callPackage ./packages/rofi-network-manager { };
              })
              parinfer-rust.overlays.default
            ];
            config = {
              allowUnfree = true;
            };
          };
        in
        {
          _module.args.pkgs = pkgs;
          formatter = pkgs.alejandra;
        };
    };
}
