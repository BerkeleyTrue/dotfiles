{
  description = "My taffybar config";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self inputs; } {

      systems = [ "x86_64-linux" "x86_64-darwin" ];
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { self', config, system, pkgs, lib, ... }:
       let
        hPkgs = pkgs.haskell.packages."ghc945";
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack-wrapped";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        watch-compile = pkgs.writeShellScriptBin "watch-compile" ''
          set -x
          stack build --fast --file-watch
        '';

        compile-taffybar = pkgs.writeShellScriptBin "compile-taffybar" ''
          set -x
          stack build
          stack install --local-bin-path bin/
          stack install
          systemctl --user restart taffybar
        '';

        devTools = with pkgs; [
          stack-wrapped
          watch-compile
          compile-taffybar

          hPkgs.haskell-language-server
          hpack

          cairo
          gcc
          glib
          zlib
          gtk3
          gobject-introspection
          libdbusmenu
          libdbusmenu-gtk3
          xorg.libICE
          xorg.libSM
          xorg.libX11
          xorg.libXScrnSaver
          xorg.libXext
          xorg.libXinerama
          xorg.libXrandr
          xorg.libXrender
          xorg.libxcb
        ];
      in {
        devShells.default = pkgs.mkShell {
          name = "taffybar";
          buildInputs = devTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;

          shellHook = ''
            zsh
            exit 0
          '';
        };

      };
  };
}
