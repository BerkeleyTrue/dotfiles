{
  inputs,
  lib,
  ...
}: {
  home-manager-parts = {
    inherit (inputs) home-manager;
    enable = true;
    exposePackages = true;

    defaults = {
      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      stateVersion = "22.11";
      system = "x86_64-linux";
    };

    shared = {
      pkgs,
      profile,
      ...
    }: let
      theme = import ../theme {};

      nixGLWrap = import ../lib/nixGL.nix {
        inherit lib pkgs;
      };
    in {
      extraSpecialArgs = {inherit theme nixGLWrap profile;};
    };

    profiles = {
      # main workstation
      delora = {
        username = "berkeleytrue";
        modules = [
          ./modules
        ];

        specialArgs = {
          hardware.monitors = {
            g5 = {
              height = 1440;
              width = 3440;
            };
            dell = {
              height = 1080;
              width = 2560;
            };
          };
        };
      };

      # framework laptop
      rena = {
        username = "bt";
        modules = [
          ./modules
        ];

        specialArgs = {
          hardware.monitors = {
            framework = {
              height = 1504;
              width = 2560;
            };
          };
        };
      };
    };
  };
}
