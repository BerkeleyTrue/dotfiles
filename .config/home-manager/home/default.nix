{
  inputs,
  lib,
  ...
}: {
  home-manager-parts = {
    inherit (inputs) home-manager;
    enable = true;
    exposePackages = true;

    shared = {profile, ...}: let
      nixgl = inputs.nixgl;
      theme = import ../theme {};
      kdl = import ../lib/kdl.nix {
        inherit lib;
      };
    in {
      modules = [
        inputs.flatpak.homeManagerModules.nix-flatpak
        inputs.catppuccin.homeModules.catppuccin
        inputs.pam-shim.homeModules.default
      ];
      extraSpecialArgs = {
        inherit theme profile nixgl kdl;
      };
    };
  };
}
