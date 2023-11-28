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

  home.nix-wallpapers = {
    g5 = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.mauve;
        endColor = theme.colors.sapphire;
      };
      colors = {
        color0 = theme.colors.overlay0;
        color1 = theme.colors.overlay0;
        color2 = theme.colors.overlay0;
        color3 = theme.colors.overlay0;
        color4 = theme.colors.overlay0;
        color5 = theme.colors.overlay0;
      };
      height = 1440;
      width = 3440;
    };

    dell = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.lavender;
        endColor = theme.colors.sapphire;
      };
      colors = {
        color0 = theme.colors.subtext1;
        color1 = theme.colors.subtext1;
        color2 = theme.colors.subtext1;
        color3 = theme.colors.subtext1;
        color4 = theme.colors.subtext1;
        color5 = theme.colors.subtext1;
      };
      height = 1080;
      width = 2560;
    };

    framework = {
      angle = 60;
      swirl = 360;
      gradient = {
        beginColor = theme.colors.sapphire;
        endColor = theme.colors.lavender;
      };
      colors = {
        color0 = theme.colors.text;
        color1 = theme.colors.text;
        color2 = theme.colors.text;
        color3 = theme.colors.text;
        color4 = theme.colors.text;
        color5 = theme.colors.text;
      };
      height = 1504;
      width = 2256;
    };
  };
}
