{self, ...}: {
  flake.modules.homeManager.dev = {
    imports = with self.homeManager; [
      cli-tools
      neovim
      notes-sync
      taskwarrior
      tmux
    ];
  };
}
