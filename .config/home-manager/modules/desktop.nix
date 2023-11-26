{
  pkgs,
  theme,
  ...
}: {
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

  home.nix-wallpapers = let
    colors = {
      color0 = theme.colors.mauve;
      color1 = theme.colors.sapphire;
      color2 = theme.colors.pink;
      color3 = theme.colors.teal;
      color4 = theme.colors.lavender;
      color5 = theme.colors.blue;
    };
  in {
    g5 = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.mauve;
        endColor = theme.colors.sapphire;
      };
      colors = colors;
      height = 1440;
      width = 3440;
    };
    dell = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.pink;
        endColor = theme.colors.blue;
      };
      colors = colors;
      height = 1080;
      width = 2560;
    };
  };
}
