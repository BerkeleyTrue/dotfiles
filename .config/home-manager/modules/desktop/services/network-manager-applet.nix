{pkgs, ...}: {
  home.packages = with pkgs; [
    networkmanagerapplet
  ];

  systemd.user.services.network-manager-applet = {
    Unit = {
      Description = "Network Manager applet";
      Requires = "tray.target";
      After = ["graphical-session-pre.target" "tray.target"];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet - -sm-disable - -indicator";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
