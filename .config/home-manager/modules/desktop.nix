{pkgs, ...}: {
  imports = [
    ./desktop
  ];

  home.packages = with pkgs; [
    dracula-icon-theme # Dracula icon theme
    papirus-icon-theme # Papirus icon theme (dracula does not have nixos icon)
    nitrogen # wallpaper manager
    nodePackages.clipboard-cli # Access the system clipboard (copy/paste) from the command-line
    xclip # x11 clipboard cli
    xplugd # A UNIX daemon that executes a script on X input and RandR changes
    yad # A fork of Zenity with many improvements
  ];
}
