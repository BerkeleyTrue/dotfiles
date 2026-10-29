{self, ...}: {
  flake.modules.homeManager.base = {
    imports = with self.modules.homeManager; [
      attic
      awww
      catppuccin
      desktop
      dev
      fonts
      monitor
      nix
      nixgl
      sops
    ];
  };
}
