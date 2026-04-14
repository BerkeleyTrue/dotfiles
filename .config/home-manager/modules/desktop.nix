{self, ...}: {
  flake.modules.homeManager.desktop = {
    imports = with self.homeManager; [
      mako
      flatpak
    ];
  };
}
