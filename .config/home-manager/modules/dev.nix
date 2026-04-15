{self, ...}: {
  flake.modules.homeManager.dev = {
    imports = with self.homeManager; [
      cli-tools
      neovim
      gh
      notes-sync
      taskwarrior
      tmux
    ];
  };
}
