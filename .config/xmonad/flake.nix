{
  description = "My Xmonad config";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    boulder.url = "github:berkeleytrue/nix-boulder-banner";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit self inputs;} {
      systems = ["x86_64-linux" "x86_64-darwin"];
      imports = [
        inputs.boulder.flakeModule
      ];

      perSystem = {
        self',
        config,
        system,
        pkgs,
        lib,
        ...
      }: let
        hPkgs = pkgs.haskell.packages."ghc945";
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack-wrapped";
          paths = [pkgs.stack];
          buildInputs = [pkgs.makeWrapper];
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
          # create a symbolic link, force it to overwrite if it already exists (-f),
          # and force it to be a real file (-T)
          # this will allow xmonad to be restarted without having to recompile
          ln -f -T bin/xmonad $HOME/.local/bin/xmonad-x86_64-linux
        '';

        buildInputs = with pkgs; [
          stack-wrapped
          watch-compile
          compile-xmonad

          hPkgs.haskell-language-server
          hpack

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
        boulder.commands = [
          {
            exec = watch-compile;
            description = "Watch and compile";
            category = "development";
          }
          {
            exec = compile-xmonad;
            description = "Compile xmonad, copy to ~/.local/bin/xmonad-x86_64-linux, and restart xmonad";
            category = "development";
          }
        ];

        devShells.default = pkgs.mkShell {
          name = "xmonad";
          inputsFrom = [
            config.boulder.devShell
          ];

          buildInputs = buildInputs;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;

          shellHook = ''
            zsh
            exit 0
          '';
        };
      };
    };
}
