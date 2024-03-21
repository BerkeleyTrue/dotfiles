{pkgs, ...}: {
  services.flatpak = {
    enable = true;
    packages = [
      "com.uploadedlobster.peek"
    ];
  };
}
