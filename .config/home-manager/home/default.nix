{
  inputs,
  lib,
  ...
}: {
  profile-parts.default.home-manager = {
    inherit (inputs) home-manager nixpkgs;

    username = "berkeleytrue";
    exposePackages = true;
  };

  profile-parts.global.home-manager = {pkgs, profile, ...}: let
    theme = import ../theme {};

    nixGLWrap = import ../lib/nixGL.nix {
      inherit lib pkgs;
    };
  in {
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
