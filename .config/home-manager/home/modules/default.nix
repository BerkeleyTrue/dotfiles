{...}: {
  imports = [
    ../../lib/modules/programs/rofi-network-manager.nix
    ../../lib/modules/nix-wallpaper
    ./commandline.nix
    ./dev.nix
    ./apps.nix
    ./desktop.nix
    ./fonts.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
