{pkgs, ...}: {
  home.packages = [
    pkgs.flameshot
  ];

  services.flameshot = {
    enable = true;
  };
}
