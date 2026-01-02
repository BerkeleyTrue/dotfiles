{...}: {
  imports = [
    ./niri
    ./services
    ./waybar
    ./hyprlock.nix
  ];

  # Enable PAM shim to support auth in non-nixos environments
  pamShim.enable = true;
}
