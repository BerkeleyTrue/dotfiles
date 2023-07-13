{
  description = "My Xmonad config";
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
          set -ex
          stack build --fast --file-watch
        '';

        compile-xmonad = pkgs.writeShellScriptBin "compile-xmonad" ''
          set -ex

          stack build :xmonad
          stack install :xmonad --local-bin-path bin/
          stack install
          cp bin/xmonad $HOME/.local/share/xmonad/xmonad-x86_64-linux
          cp bin/xmonad $HOME/.local/bin/xmonad-x86_64-linux &> /dev/null # errors when xmonad is already running
        '';

        devTools = with pkgs; [
          stack-wrapped
          watch-compile
          compile-xmonad
          hpack

          hPkgs.haskell-language-server

          gcc
          zlib
          xorg.libX11
          xorg.libXft
          xorg.libXScrnSaver
          xorg.libXext
          xorg.libXinerama
          xorg.libXrandr
        ];
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = devTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;

          shellHook = ''
            export NIX_SHELL_NAME="xmonad"
            zsh
            exit 0
          '';
        };

      };
  };
}
