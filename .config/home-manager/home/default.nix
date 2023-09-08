{
  inputs,
  lib,
  ...
}: {
  home-manager-parts = {
    defaults = {
      inherit (inputs) home-manager nixpkgs;

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
      specialArgs = {inherit inputs theme nixGLWrap;};
    };

    profiles = {
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
  };
}
