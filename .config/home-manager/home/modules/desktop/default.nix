{...}: {
  imports = [
    ./services
    ./niri
    # ./dunst.nix # using mako instead
    ./waybar.nix
    ./anyrun.nix
  ];

  # Enable PAM shim to support auth in non-nixos environments
  pamShim.enable = true;
}
