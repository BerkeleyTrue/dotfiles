{ pkgs, ... }:
{
  imports = [
    ./desktop
  ];

  home.packages = with pkgs; [
    haskellPackages.status-notifier-item # sni system tray protocol
    nitrogen # wallpaper manager
    xclip # x11 clipboard cli
    xplugd # A UNIX daemon that executes a script on X input and RandR changes
    yad # A fork of Zenity with many improvements
  ];
}