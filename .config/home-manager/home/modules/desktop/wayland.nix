{pkgs, ...}: {
  home.packages = with pkgs; [
    niri
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
    configPackages = with pkgs; [
      niri
    ];
  };

  systemd.user.targets.niri-session = {
    Unit = {
      Description = "Niri WM";
      Documentation = "man:systemd.special(7)";
      BindsTo = ["graphical-session.target"];
      Wants = ["graphical-session-pre.target"];
      After = ["graphical-session-pre.target"];
    };
  };
}
