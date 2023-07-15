{ inputs, lib, withSystem, ... }:
let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  desktop = "berkeleytrue";
  laptop = "bt";
  theme = import ./theme { };
  mkHome = { system, user }:
    withSystem system ({ pkgs, ... }:
      let
        nixGLWrap = import ./lib/nixGL.nix { inherit pkgs lib; };
      in
      homeManagerConfiguration {
        inherit pkgs;

        modules = [
          ./base.nix
        ];

        extraSpecialArgs = {
          inherit user theme nixGLWrap;
        };
      });
in
{
  flake = {
    homeConfigurations.${desktop} = mkHome { user = desktop; system = "x86_64-linux"; };
    homeConfigurations.${laptop} = mkHome { user = laptop; system = "x86_64-linux"; };
  };
}
