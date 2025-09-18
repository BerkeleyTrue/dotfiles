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

  systemd.user.services.niri = {
    Unit = {
      Description = "Niri WM";
      BindsTo = ["graphical-session.target"];
      Before = ["graphical-session.target"];
      Wants = ["wayland-session.target"];
      After = ["wayland-session.target"];
    };

    Service = {
      Slice = "session.slice";
      Type = "notify";
      ExecStart = "${pkgs.niri}/bin/niri --session";
    };

    Install = {
      WantedBy = ["wayland-session.target"];
    };
  };
}
