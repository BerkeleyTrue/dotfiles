{...}: {
  imports = [
    ./services
    ./niri
    ./waybar.nix
  ];

  # Enable PAM shim to support auth in non-nixos environments
  pamShim.enable = true;
}
