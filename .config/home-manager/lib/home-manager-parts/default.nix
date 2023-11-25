# original source: https://github.com/adamcstephens/profile-parts
{
  config,
  lib,
  withSystem,
  ...
}: let
  defaults = config.home-manager-parts.defaults;
  globals = config.home-manager-parts.global;
in
  with lib; {
    options = {
      home-manager-parts.defaults = {
        home-manager = mkOption {
          type = types.unspecified;
          description = mdDoc "home-manager input to use for building all homeManagerConfigurations. Required";
        };

        enable = lib.mkOption {
          type = types.bool;
          description = mdDoc "Whether all homeManagerConfigurations should be enabled by default";
          default = true;
        };

        system = mkOption {
          type = types.enum platforms.all;
          description = mdDoc "The default system to use for building homeManagerConfigurations";
          default = "x86_64-linux";
        };

        exposePackages = mkOption {
          type = types.bool;
          description = mdDoc "Whether to expose homeManagerConfigurations output at `.#packages.<system>.home/<profile name>`";
          default = true;
        };
      };

      home-manager-parts.global = mkOption {
        type = types.functionTo (types.submodule {
          options = {
            modules = mkOption {
              type = types.listOf types.unspecified;
              description = mdDoc "List of modules to include in all homeManagerConfigurations";
              default = [];
            };

            specialArgs = mkOption {
              type = types.attrsOf types.unspecified;
              description = mdDoc "`extraSpecialArgs` passed to all homeManagerConfigurations";
              default = {};
            };
          };
        });
        description = mdDoc "Global options for all homeManagerConfigurations, a function supplied name, profile and pkgs";
      };

      home-manager-parts.profiles = mkOption {
        type = types.attrsOf (types.submodule ({
          name, # Key of the profile?
          config,
          ...
        }: let
          profile = config;
        in {
          options = {
            enable = mkOption {
              type = types.bool;
              description = mdDoc "Whether to expose the homeManagerConfiguration to the flake";
              default = defaults.enable;
            };

            username = mkOption {
              type = types.str;
              description = mdDoc "The username passed to home-manager, or `home.username`. Defaults to the profile name";
              default = name;
            };

            hostname = mkOption {
              type = types.str;
              default =
                if profile.username != name
                then name
                else "";
            };

            home-manager = mkOption {
              type = types.unspecified;
              description = mdDoc "home-manager input to use for building the homeManagerConfiguration. Required to be set per-profile or using `defaults.home-manager`";
              default = defaults.home-manager;
            };

            modules = mkOption {
              type = types.listOf types.unspecified;
              description = mdDoc "List of modules to include in the homeManagerConfiguration";
              default = [];
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

            stateVersion = mkOption {
              type = types.int;
              description = mdDoc "The stateVersion passed to home-manager, or `home.stateVersion`";
              default = defaults.stateVersion;
            };

            directory = mkOption {
              type = types.str;
              description = mdDoc "The home directory passed to home-manager, or `home.homeDirectory`";
              default = "/home/${profile.username}";
            };

            # readOnly

            homeConfigOutput = mkOption {
              type = types.unspecified;
              readOnly = true;
              description = mdDoc "Output of homeConfigurations call for this profile";
            };

            finalModules = mkOption {
              type = types.unspecified;
              description = mdDoc "Final set of modules available to be used in homeConfigurations input";
              readOnly = true;
            };

            activationPackage = mkOption {
              type = types.unspecified;
              description = mdDoc "Package to be added to the flake to provide schema-supported access to activationPackage";
              readOnly = true;
            };
          };

          config = let
            pkgs = withSystem profile.system ({pkgs, ...}: pkgs);
            globalConfig =
              if lib.isFunction globals
              then
                globals {
                  inherit pkgs;
                  profile = name;
                }
              else throw "home-manager-parts.global must be a function";
          in
            lib.mkIf profile.enable {
              finalModules =
                globalConfig.modules
                ++ profile.modules
                ++ [
                  {
                    home.stateVersion = lib.mkDefault profile.stateVersion;
                    home.homeDirectory = lib.mkDefault profile.directory;
                    home.username = lib.mkDefault profile.username;
                  }
                ];

              homeConfigOutput = profile.home-manager.lib.homeManagerConfiguration {
                inherit pkgs;

                extraSpecialArgs = lib.recursiveUpdate globalConfig.specialArgs profile.specialArgs;

                modules = profile.finalModules;
              };

              activationPackage = {
                ${profile.system}."activate-${name}" = profile.homeConfigOutput.activationPackage;
              };
            };
        }));

        description = lib.mdDoc "An attribute set of profiles to be built using homeConfigurations, where the key is the username and the value is a set of options. Each profile is built using the following options:";
      };
    };

    config = let
      # homeConfigurations.<profile> = <homeManagerConfiguration>
      homesByProfile = builtins.mapAttrs (_: profile: profile.homeConfigOutput) config.home-manager-parts.profiles;
      # homeConfigurations.<username>@<hostname> = <homeManagerConfiguration>
      homesByUsernameAtHostname =
        builtins.foldl'
        (acc: profile:
          if profile.enable && profile.hostname != ""
          then acc // {"${profile.username}@${profile.hostname}" = profile.homeConfigOutput;}
          else acc) {}
        (builtins.attrValues config.home-manager-parts.profiles);

      # group checks into system-based sortings
      # packages.<system>."home/<name>" = homeConfigurationOutput.activationPackage
      packages = lib.zipAttrs (builtins.attrValues (lib.mapAttrs (_: i: i.activationPackage) config.home-manager-parts.profiles));
    in {
      flake.homeConfigurations = homesByProfile // homesByUsernameAtHostname;

      perSystem = {system, ...}: {
        packages = lib.mkIf (defaults.exposePackages && (builtins.hasAttr system packages)) (lib.mkMerge packages.${system});
      };
    };
  }
