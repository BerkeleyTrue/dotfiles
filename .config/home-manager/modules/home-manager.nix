{
  flake.modules.homeManager.home-manager = {
    home.stateVersion = "22.11";
    system = "x86_64-linux";
    programs.home-manager.enable = true;
  };
}
