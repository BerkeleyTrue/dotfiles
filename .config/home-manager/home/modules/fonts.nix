{pkgs, ...}: {
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    ipafont
    fira-code
    fira-code-symbols # adds ligatures to fira-code in private unicode space
    fira-code-nerdfont
    dejavu_fonts
    twemoji-color-font
    roboto
  ];
}
