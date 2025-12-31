{...}: {
  imports = [
    ./niri
    ./services
    ./waybar
  ];

  # Enable PAM shim to support auth in non-nixos environments
  pamShim.enable = true;
}
