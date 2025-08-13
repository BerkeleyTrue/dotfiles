# framework laptop
{pkgs, ...}: {
  nixGL.defaultWrapper = "mesaPrime";

  home.packages = with pkgs; [
    nvtopPackages.intel
  ];

  nixGLPackage = "intel";
}
