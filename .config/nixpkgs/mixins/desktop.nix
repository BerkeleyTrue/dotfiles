{ pkgs, ... }:
{
  home.packages = with pkgs; [
    dunst # notification daemon
    flameshot # screenshot util
    nitrogen # wallpaper manager
    picom # X11 compositor
    unclutter # hide mouse when not in use
    xclip # x11 clipboard cli
    xplugd # A UNIX daemon that executes a script on X input and RandR changes
    yad # A fork of Zenity with many improvements
  ];
}
