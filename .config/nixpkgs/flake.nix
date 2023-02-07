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

  outputs = { self, nixpkgs, home-manager, nur, ... }:
    let
      system = "x86_64-linux";
      user = "berkeleytrue";
      pkgs = import nixpkgs {
        inherit system;
      #   overlays = [
      #     (import ./overlays.nix {}).overlays
        # ];
      };
      # nextUser = "bt";
      # location = "$HOME/.config/flake";
    in {
      homeConfigurations.${user} = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          ./home.nix
        ];

        extraSpecialArgs = {
          inherit user nur;
        };
      };
    };
}
