{self, ...}: {
  flake.modules.homeManager.dev = {
    imports = with self.modules.homeManager; [
      cli-tools
      neovim
      gh
      notes-sync
      taskwarrior
      tmux
    ];
  };
}
