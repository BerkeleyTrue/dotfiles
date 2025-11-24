# framework laptop
{pkgs, ...}: {
  targets.genericLinux.nixGL.defaultWrapper = "mesaPrime";

  home.packages = with pkgs; [
    nvtopPackages.intel
  ];

  nixGLPackage = "intel";
}
