{ pkgs, ... }:
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
      gparted # graphical partition manager
      rofi-calc # calculator
      viewnior # fast image preview
      zathura # pdf viewer
      keybase # encrypted chat
      keybase-gui # encrypted chat
      rofi # launcher
    ];
}
