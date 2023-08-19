{ pkgs, nixGLWrap, ... }:
let
  rofi = pkgs.rofi.override {
    plugins = with pkgs; [
      rofi-calc
      rofi-emoji
    ];
  };

  aspell = (pkgs.aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]));

  rofi-spell = pkgs.writeShellScriptBin "rofi-spell" ''
    word=$(${aspell}/bin/aspell -d en dump master | ${aspell}/bin/aspell -l en expand | rofi -p 'spell' -dmenu)
    definition=$(${pkgs.wordnet}/bin/wn $word -over)

    if [[ ! -z "$definition" ]]; then
      rofi -p -e "$definition"
    fi

    echo -n $word | ${pkgs.nodePackages.clipboard-cli}/bin/clipboard
    echo $word
  '';
in
{
  home.packages = with pkgs;
    [
      (nixGLWrap alacritty) # GPU-accelerated terminal emulator
      (nixGLWrap kitty) # GPU-accelerated terminal emulator
      (nixGLWrap mpv) # General-purpose media player, fork of MPlayer and mplayer2
      aspell # spell checker
      gparted # graphical partition manager
      keybase # encrypted chat
      keybase-gui # encrypted chat
      postman # API Development Environment
      rofi # launcher
      rofi-spell # spell checker
      rofi-bluetooth # rofi bluetooth manager
      viewnior # fast image preview
      vlc # Cross-platform media player and streaming server
      zathura # pdf viewer
    ];

  services.keybase = {
    enable = true;
  };

  services.kbfs = {
    enable = true;
    mountPoint = "docs/keybase";
  };
}
