{pkgs, ...}: {
  home.packages = with pkgs; [
    blueman
  ];

  # requires system installation of blueman
  systemd.user.services.blueman-applet = {
    Unit = {
      Description = "Blueman Bluetooth Applet";
      Documentation = ["man:blueman-applet(1)"];
      After = ["tray.target"];
      PartOf = ["tray.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.blueman}/bin/blueman-applet";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };
}
