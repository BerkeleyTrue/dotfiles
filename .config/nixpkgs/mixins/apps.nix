{ pkgs, ... }:
{
  home.packages = with pkgs; [
    gparted # graphical partition manager
    rofi # dmenu replacement
    rofi-calc # calculator
    viewnior # fast image preview
    zathura # pdf viewer
    keybase # encrypted chat
    keybase-gui # encrypted chat
  ];
}
