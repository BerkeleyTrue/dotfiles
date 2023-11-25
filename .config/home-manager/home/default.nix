{
  inputs,
  lib,
  ...
}: {
  home-manager-parts = {
    defaults = {
      inherit (inputs) home-manager;

      exposePackages = true;
    };

    global = {
      pkgs,
      profile,
      ...
    }: let
      theme = import ../theme {};

      nixGLWrap = import ../lib/nixGL.nix {
        inherit lib pkgs;
      };
    in {
      specialArgs = {inherit inputs theme nixGLWrap profile;};
    };

    profiles = {
      # main workstation
      delora = {
        username = "berkeleytrue";
        modules = [
          ../base.nix
        ];
      };

      # framework laptop
      rena = {
        username = "bt";
        modules = [
          ../base.nix
        ];
      };
    };
  };
}
