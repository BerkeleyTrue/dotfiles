{
  description = "My special snow flake";

  inputs = {
    nixpkgs.url = "github:nixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flatpak.url = "github:gmodena/nix-flatpak";

    catppuccin.url = "github:catppuccin/nix";
    catppuccin.inputs.nixpkgs.follows = "nixpkgs";

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
      perSystem = {system, ...}: let
        pkgs = import inputs.nixpkgs {
          inherit system;

          overlays = [
            inputs.nixgl.overlay
            inputs.parinfer-rust.overlays.default
            (import ./overlays/rofi-network-manager)
            (import ./overlays/crush)
          ];

          config = {
            allowUnfree = true;
            permittedInsecurePackages = [
              "nix-2.16.2"
            ];
          };
        };
      in {
        formatter = pkgs.alejandra;
        _module.args.pkgs = pkgs;
        devShells.default = pkgs.mkShell {
          name = "home-manager";
          buildInputs = with pkgs; [
            just
          ];
          shellHook = ''
            function menu () {
              echo
              echo -e "\033[1;34m>==> ️  '$name'\n\033[0m"
              ${pkgs.just}/bin/just --list
              echo
              echo "(Run 'just --list' to display this menu again)"
              echo
            }

            menu
          '';
        };
      };
    };
}
