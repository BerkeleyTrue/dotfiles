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

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders;
    };
  };

  home.packages = with pkgs; [
    # papirus-icon-theme # Papirus icon theme (dracula does not have nixos icon, comes included in catppuccin-papirus-folders)
    catppuccin-papirus-folders # catppuccin papirus folder theme
    catppuccin-cursors.frappeLavender # # catppuccin cursor theme
    dracula-icon-theme # Dracula icon theme
    hyprpicker # color picker that does not suck
    powermenu-rs # powermenu written in rust with relm4/gtk4
    wev # wayland event viewer
    wl-clipboard-rs # wayland clipboard cli
  ];
}
