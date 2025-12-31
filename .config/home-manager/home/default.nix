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

    profiles = {
      # main workstation
      delora = {
        username = "berkeleytrue";
        modules = [
          ./modules
          ./hardware/delora.nix
        ];

        specialArgs = {
          hardware.monitors = {
            g5 = {
              height = 1440;
              width = 3440;
              label = "HDMI-A-1";
              rate = 165;
              position = {
                x = 0;
                y = 720;
              };
            };
            dell = {
              height = 1080;
              width = 2560;
              label = "DP-3";
              rate = 60;
              position = {
                x = 0;
                y = 0;
              };
            };
          };
        };
      };

      # framework laptop
      rena = {
        username = "bt";
        modules = [
          ./modules
          ./hardware/rena.nix
        ];

        specialArgs = {
          hardware.monitors = {
            framework = {
              height = 1504;
              width = 2256;
              label = "eDP-1";
              rate = 60;
            };
          };
        };
      };
    };
  };
}
