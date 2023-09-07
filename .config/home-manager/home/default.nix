{
  inputs,
  lib,
  withSystem,
  ...
}:
with lib; {
  profile-parts.default.home-manager = {
    inherit (inputs) home-manager nixpkgs;

    username = "berkeleytrue";
    exposePackages = false;
  };

  profile-parts.global.home-manager = {
    modules = {profile, ...}: let
      pkgs = mkForce (import profile.nixpkgs {
        inherit (profile) system;

        overlays = [
          inputs.nixgl.overlay
          inputs.parinfer-rust.overlays.default
          (import ../overlays/rofi-network-manager)
        ];

        config = {
          allowUnfree = true;
        };
      });
    in [
      {
        _module.args.pkgs = pkgs;
      }
    ];

    specialArgs = {profile, ...}:
      withSystem profile.system ({
        pkgs,
        ...
      }: let
        theme = import ../theme {};
        nixGLWrap = import ./lib/nixGL.nix {inherit pkgs lib;};
      in {
        inherit inputs;

        specialArgs = {
          inherit theme nixGLWrap;
        };
      });
  };

  profile-parts.home-manager = {
    desktop = {
      modules = [
        ../base.nix
      ];
    };
  };
}
