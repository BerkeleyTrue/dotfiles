{pkgs, ...}: {
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    ipafont
    fira-code
    fira-code-symbols # adds ligatures to fira-code in private unicode space
    (pkgs.nerdfonts.override {fonts = ["FiraCode"];})
    dejavu_fonts
    twemoji-color-font
    roboto
  ];
}
