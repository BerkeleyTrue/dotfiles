{
  pkgs,
  nixgl,
  ...
}: {
  targets.genericLinux.nixGL.packages = nixgl.packages;

  imports = [
    ./desktop
  ];

  home.pointerCursor = {
    name = "catppuccin-frappe-lavender-cursors";
    package = pkgs.catppuccin-cursors.frappeLavender;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

  home.packages = with pkgs; [
    dracula-icon-theme # Dracula icon theme
    # papirus-icon-theme # Papirus icon theme (dracula does not have nixos icon, comes included in catppuccin-papirus-folders)
    catppuccin-papirus-folders # catppuccin papirus folder theme
    catppuccin-cursors.frappeLavender # # catppuccin cursor theme
    # nitrogen # wallpaper manager
    wev # wayland event viewer
    wl-clipboard-rs # wayland clipboard cli
    mako # wayland notification daemon
  ];
}
