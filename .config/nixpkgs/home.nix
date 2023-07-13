{ pkgs, user, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = "/home/${user}";

  imports = [
    ./mixins/commandline.nix
    ./mixins/dev.nix
    ./mixins/apps.nix
    ./mixins/desktop.nix
  ];

  home.packages = with pkgs; [
    alacritty # GPU-accelerated terminal emulator
    kitty # GPU-accelerated terminal emulator
    haskellPackages.status-notifier-item # sni system tray protocol
    mpv-unwrapped # General-purpose media player, fork of MPlayer and mplayer2
    postman # API Development Environment
    vlc # Cross-platform media player and streaming server
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
