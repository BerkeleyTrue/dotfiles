{pkgs, ...}: {
  home.packages = with pkgs; [
    nvtopPackages.amd
  ];
}
