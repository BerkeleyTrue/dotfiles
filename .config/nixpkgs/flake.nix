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

  };

  outputs = { nixpkgs, home-manager, nur, ... }:
    let
      system = "x86_64-linux";
      desktop = "berkeleytrue";
      laptop = "bt";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };
      mkHome = { system ? system, user }: home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          ./home.nix
        ];

        extraSpecialArgs = {
          inherit user nur;
        };
      };
    in
    {
      homeConfigurations.${desktop} = mkHome { user = desktop; };
      homeConfigurations.${laptop} = mkHome { user = laptop; };
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
