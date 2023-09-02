{pkgs, ...}: {
  systemd.user.services.xplugd = {
    Unit = {
      Description = "Rerun after IO has changed using xplugd";
      After = ["graphical-session-pre.target"];
      PartOf = "graphical-session.target";
    };

    Service = {
      Type = "forking";
      ExecStart = "${pkgs.xplugd}/bin/xplugd";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
