{
  description = "My special snow flake";

  inputs = {
    nixpkgs.url = "github:nixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # utils
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    home-manager-parts.url = "github:berkeleytrue/home-manager-parts";

    nixgl.url = "github:guibou/nixGL";
    nixgl.inputs.nixpkgs.follows = "nixpkgs";

    parinfer-rust.url = "github:PhilTaken/parinfer-rust";
    parinfer-rust.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        inputs.home-manager-parts.flakeModule
        ./home
      ];
      perSystem = {
        pkgs,
        system,
        ...
      }:
        with inputs; let
          pkgs = import nixpkgs {
            inherit system;

            overlays = [
              nixgl.overlay
              parinfer-rust.overlays.default
              (import ./overlays/rofi-network-manager)
            ];

            config = {
              allowUnfree = true;
            };
          };
        in {
          formatter = pkgs.alejandra;
          _module.args.pkgs = pkgs;
        };
    };
}
