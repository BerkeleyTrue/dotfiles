# original source: https://github.com/adamcstephens/profile-parts
{
  config,
  lib,
  withSystem,
  ...
}: let
  defaults = config.profile-parts.default.home-manager;
  globals = config.profile-parts.global.home-manager;
in
  with lib; {
    options = {
      profile-parts.default.home-manager = {
        enable = lib.mkOption {
          type = types.bool;
          description = mdDoc "Whether all homeManagerConfigurations should be enabled by default";
          default = true;
        };

        exposePackages = mkEnableOption (mdDoc "Expose all homeManagerConfigurations at `.#packages.<system>.home/<profile name>`");

        home-manager = mkOption {
          type = types.unspecified;
          description = mdDoc "home-manager input to use for building all homeManagerConfigurations. Required";
        };

        nixpkgs = mkOption {
          type = types.unspecified;
          description = mdDoc "The default nixpkgs input to use for building homeManagerConfigurations. Required";
        };

        system = mkOption {
          type = types.enum platforms.all;
          description = mdDoc "The default system to use for building homeManagerConfigurations";
          default = "x86_64-linux";
        };

        username = mkOption {
          type = types.nullOr types.str;
          description = mdDoc "The default username passed to home-manager, or `home.username`. If unset, profiles will use their attribute name.";
          default = null;
        };
      };

      profile-parts.global.home-manager = mkOption {
        type = types.functionTo (types.submodule {
          options = {
            modules = mkOption {
              type = types.listOf types.unspecified;
              description = mdDoc "List of modules to include in all homeManagerConfigurations. Can also be a function that will be passed the `name` and `profile`";
              default = [];
            };

            specialArgs = mkOption {
              type = types.attrsOf types.unspecified;
              description = mdDoc "`extraSpecialArgs` passed to all homeManagerConfigurations";
              default = {};
            };
          };
        });
        description = mdDoc "Global options for all homeManagerConfigurations, a function supplied name and profile";
      };

      profile-parts.home-manager = mkOption {
        type = types.attrsOf (types.submodule ({
          name,
          config,
          ...
        }: {
          options = {
            enable = mkOption {
              type = types.bool;
              description = mdDoc "Whether to expose the homeManagerConfiguration to the flake";
              default = defaults.enable;
            };

            directory = mkOption {
              type = types.str;
              description = mdDoc "The home directory passed to home-manager, or `home.homeDirectory`";
              default =
                if config.nixpkgs.legacyPackages.${config.system}.stdenv.isDarwin
                then "/Users/${config.username}"
                else "/home/${config.username}";
            };

            home-manager = mkOption {
              type = types.unspecified;
              description = mdDoc "home-manager input to use for building the homeManagerConfiguration. Required to be set per-profile or using `default.home-manager.home-manager`";
              default = defaults.home-manager;
            };

            modules = mkOption {
              type = types.listOf types.unspecified;
              description = mdDoc "List of modules to include in the homeManagerConfiguration";
              default = [];
            };

            nixpkgs = mkOption {
              type = types.unspecified;
              description = mdDoc "nixpkgs input to use for building the homeManagerConfiguration. Required to be set per-profile or using `default.home-manager.nixpkgs";
              default = defaults.nixpkgs;
            };

            specialArgs = mkOption {
              type = types.attrsOf types.unspecified;
              description = mdDoc "`extraSpecialArgs` passed to the homeManagerConfiguration";
              default = {};
            };

            system = mkOption {
              type = types.enum platforms.all;
              description = mdDoc "system used for building the homeManagerConfiguration";
              default = defaults.system;
            };

            username = mkOption {
              type = types.str;
              description = mdDoc "The username passed to home-manager, or `home.username`. Defaults to default username if set, otherwise reads from the profile name";
              default =
                if (defaults.username == null)
                then name
                else defaults.username;
            };

            # readOnly

            finalHome = mkOption {
              type = types.unspecified;
              readOnly = true;
            };

            finalModules = mkOption {
              type = types.unspecified;
              description = mdDoc "Final set of modules available for ";
              readOnly = true;
            };

            finalPackage = mkOption {
              type = types.unspecified;
              description = mdDoc "Package to be added to the flake to provide schema-supported access to activationPackage";
              readOnly = true;
            };
          };

          config = let
            profile = config;
            globalConfig =
              if lib.isFunction globals
              then globals {inherit name profile;}
              else throw "profile-parts.global.home-manager must be a function";
          in
            lib.mkIf profile.enable {
              finalHome = withSystem profile.system ({pkgs, ...}:
                profile.home-manager.lib.homeManagerConfiguration {
                  pkgs = profile.nixpkgs.legacyPackages.${profile.system};

                  extraSpecialArgs = lib.recursiveUpdate globalConfig.specialArgs profile.specialArgs;

                  modules = profile.finalModules;
                });

              finalModules =
                globalConfig.modules
                ++ [
                  {
                    home.homeDirectory = lib.mkDefault profile.directory;
                    home.username = lib.mkDefault profile.username;
                  }
                ]
                ++ profile.modules;

              finalPackage.${profile.system}."home/${name}" = profile.finalHome.activationPackage;
            };
        }));
        description = lib.mdDoc "";
      };
    };

    config = let
      homes = builtins.mapAttrs (_: config: config.finalHome) config.profile-parts.home-manager;

      # group checks into system-based sortings
      packages = lib.zipAttrs (builtins.attrValues (lib.mapAttrs (_: i: i.finalPackage) config.profile-parts.home-manager));
    in {
      flake.homeConfigurations = homes;

      perSystem = {system, ...}: {
        packages = lib.mkIf (defaults.exposePackages && (builtins.hasAttr system packages)) (lib.mkMerge packages.${system});
      };
    };
  }
