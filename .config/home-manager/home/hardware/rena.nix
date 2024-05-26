# framework laptop
{pkgs, ...}: {
  home.packages = with pkgs; [
    nvtopPackages.intel
  ];
  
  nixGLPackage = "intel";
}
