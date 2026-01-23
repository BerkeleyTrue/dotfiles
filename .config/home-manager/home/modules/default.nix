{...}: {
  imports = [
    # ../../lib/modules/programs/rofi-network-manager.nix
    ../../lib/modules/nix-wallpaper
    ../../lib/modules/nixgl.nix
    ./apps.nix
    ./catppuccin.nix
    ./commandline.nix
    ./desktop.nix
    ./dev.nix
    ./fonts.nix
    ./neovim.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
