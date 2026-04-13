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

    # uses 25.11
    powermenu-rs.url = "github:BerkeleyTrue/powermenu-rs";

    # flake parts/dentritic
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    ## import files automatically by directory
    import-tree.url = "github:vic/import-tree";

    # flake modules but class/aspect inverted to aspect/class
    flake-aspects.url = "github:vic/flake-aspects";

    nixgl.url = "github:nix-community/nixGL";
    nixgl.inputs.nixpkgs.follows = "nixpkgs";

    parinfer-rust.url = "github:PhilTaken/parinfer-rust";
    parinfer-rust.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        (inputs.import-tree ./modules)
      ];
    };
}
