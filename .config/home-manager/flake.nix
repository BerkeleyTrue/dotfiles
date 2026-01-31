{
  description = "My special snow flake";

  inputs = {
    nixpkgs.url = "github:nixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flatpak.url = "github:gmodena/nix-flatpak";

    catppuccin.url = "github:catppuccin/nix";
    catppuccin.inputs.nixpkgs.follows = "nixpkgs";

    pam-shim.url = "github:Cu3PO42/pam_shim";
    pam-shim.inputs.nixpkgs.follows = "nixpkgs";

    awww.url = "git+https://codeberg.org/LGFae/awww";
    awww.inputs.nixpkgs.follows = "nixpkgs";

    # uses 25.11
    powermenu-rs.url = "github:BerkeleyTrue/powermenu-rs";

    # utils
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    flake-file.url = "github:vic/flake-file";
    import-tree.url = "github:vic/import-tree";
    den.url = "github:vic/den";
    flake-aspects.url = "github:vic/flake-aspects";

    home-manager-parts.url = "github:berkeleytrue/home-manager-parts";

    nixgl.url = "github:nix-community/nixGL";
    nixgl.inputs.nixpkgs.follows = "nixpkgs";

    parinfer-rust.url = "github:PhilTaken/parinfer-rust";
    parinfer-rust.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        inputs.flake-parts.flakeModules.modules
        inputs.home-manager-parts.flakeModule
        ./home
        (inputs.import-tree ./modules)
      ];
      perSystem = {system, ...}: let
        pkgs = import inputs.nixpkgs {
          inherit system;

          overlays = [
            inputs.nixgl.overlay
            inputs.parinfer-rust.overlays.default
            inputs.awww.overlays.default
            inputs.powermenu-rs.overlays.default
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
