{self, ...}: {
  flake.modules.homeManager.base = {
    imports = with self.modules.homeManager; [
      awww
      catppuccin
      cli-tools
      desktop
      dev
      fonts
      monitor
      nix
      nixgl
    ];
  };
}
