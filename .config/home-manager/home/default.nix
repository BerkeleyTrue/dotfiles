{
  inputs,
  lib,
  ...
}:
with lib; {
  profile-parts.default.home-manager = {
    inherit (inputs) home-manager nixpkgs;

    username = "berkeleytrue";
    exposePackages = true;
  };

  profile-parts.global.home-manager = {profile, ...}: let
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

    theme = import ../theme {};

    nixGLWrap = import ../lib/nixGL.nix {inherit pkgs lib;};
  in {
    modules = [
      {
        _module.args.pkgs = pkgs;
      }
    ];

    specialArgs = {inherit inputs theme nixGLWrap;};
  };

  profile-parts.home-manager = {
    berkeleytrue = {
      modules = [
        ../base.nix
      ];
    };

    bt = {
      modules = [
        ../base.nix
      ];
    };
  };
}
