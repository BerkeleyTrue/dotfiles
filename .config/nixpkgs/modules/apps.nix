{ pkgs, nixGLWrap, ... }:
let
  rofi = pkgs.rofi.override {
    plugins = with pkgs; [
      rofi-calc
      rofi-emoji
    ];
  };
in
{
  home.packages = with pkgs;
    [
      (nixGLWrap alacritty) # GPU-accelerated terminal emulator
      (nixGLWrap kitty) # GPU-accelerated terminal emulator
      (nixGLWrap mpv) # General-purpose media player, fork of MPlayer and mplayer2
      gparted # graphical partition manager
      keybase # encrypted chat
      keybase-gui # encrypted chat
      postman # API Development Environment
      rofi # launcher
      rofi-calc # calculator
      viewnior # fast image preview
      vlc # Cross-platform media player and streaming server
      zathura # pdf viewer
    ];
}
