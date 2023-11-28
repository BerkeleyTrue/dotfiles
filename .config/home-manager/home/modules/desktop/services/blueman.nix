{pkgs, ...}: {
  home.packages = with pkgs; [
    blueman
  ];

  # requires system installation of blueman
  systemd.user.services.blueman-applet = {
    Unit = {
      Description = "Blueman Applet";
      Requires = "tray.target";
      After = ["graphical-session-pre.target" "tray.target"];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "${pkgs.blueman}/bin/blueman-applet";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
