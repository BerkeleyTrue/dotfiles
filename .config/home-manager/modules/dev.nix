{self, ...}: {
  flake.modules.homeManager.dev = {
    imports = with self.modules.homeManager; [
      cli-tools
      neovim
      gh
      mimeapps-guard
      notes-sync
      taskwarrior
      tmux
    ];
  };
}
