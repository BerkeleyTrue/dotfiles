{...}: {
  imports = [
    ./lib/modules/programs/rofi-network-manager.nix
    ./lib/modules/nix-wallpaper
    ./modules/commandline.nix
    ./modules/dev.nix
    ./modules/apps.nix
    ./modules/desktop.nix
    ./modules/fonts.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
