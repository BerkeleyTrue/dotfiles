# framework laptop
{pkgs, ...}: {
  nixGL.defaultWrapper = "intel";

  home.packages = with pkgs; [
    nvtopPackages.intel
  ];
  
  nixGLPackage = "intel";
}
