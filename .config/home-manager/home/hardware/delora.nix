{pkgs, ...}: {
  nixGL.defaultWrapper = "mesa";

  home.packages = with pkgs; [
    nvtopPackages.amd
  ];

  # deprecated
  nixGLPackage = "mesa";
}
