{pkgs, ...}: {
  imports = [
    ./services
    ./niri
    ./dunst.nix
  ];

  programs.anyrun = {
    enable = true;
    config = {
      plugins = [
        "${pkgs.anyrun}/lib/libapplication.so"
        "${pkgs.anyrun}/lib/libsymbols.so"
        "${pkgs.anyrun}/lib/librink.so"
        "${pkgs.anyrun}/lib/libdictionary.so"
        "${pkgs.anyrun}/lib/libniri_focus.so"
      ];
    };
  };
}
