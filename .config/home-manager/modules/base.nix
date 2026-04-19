{self, ...}: {
  flake.modules.homeManager.base = {
    imports = with self.modules.homeManager; [
      attic
      awww
      catppuccin
      cli-tools
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
