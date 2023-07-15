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

  };

  outputs = { nixpkgs, home-manager, nur, nixgl, ... }:
    let
      system = "x86_64-linux";
      desktop = "berkeleytrue";
      laptop = "bt";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ nixgl.overlay ];
        config = {
          allowUnfree = true;
        };
      };
      theme = import ./theme { };
      mkHome = { system ? system, user }: home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          ./home.nix
        ];

        extraSpecialArgs = {
          inherit user nur theme;
        };
      };
    in
    {
      homeConfigurations.${desktop} = mkHome { user = desktop; };
      homeConfigurations.${laptop} = mkHome { user = laptop; };
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
