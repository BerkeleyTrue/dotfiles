{
  lib,
  inputs,
  config,
  withSystem,
  ...
}: {
  # # Declare flake.homeConfigurations so multiple modules can contribute
  options.flake.homeConfigurations = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.raw;
    default = {};
  };

  options.configurations.home = lib.mkOption {
    type = lib.types.lazyAttrsOf (
      lib.types.submodule {
        options.username = lib.mkOption {
          type = lib.types.str;
        };
        options.system = lib.mkOption {
          type = lib.types.str;
        };
        options.modules = lib.mkOption {
          type = lib.types.listOf lib.types.deferredModule;
        };
      }
    );
    default = {};
  };

  config.flake.homeConfigurations = lib.mergeAttrsList (
    lib.mapAttrsToList (hostname: {
      username,
      modules,
      system,
    }: let
      pkgs = withSystem system ({pkgs, ...}: pkgs);
      configOutput = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules =
          [
            {
              home.stateVersion = "22.11";
              programs.home-manager.enable = true;
            }
            {inherit system;}
          ]
          ++ modules;
      };
    in {
      "${username}@${hostname}" = configOutput;
      "${username}" = configOutput;
    })
    config.configurations.home
  );
}
