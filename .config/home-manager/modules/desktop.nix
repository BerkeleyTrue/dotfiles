{
  flake.modules.homeModule.desktop = {pkgs, ...}: {
    home.packages = with pkgs; [
      hyprpicker # color picker that does not suck
      wev # wayland event viewer
      wl-clipboard-rs # wayland clipboard cli
    ];
  };
}
