{pkgs, ...}: {
  systemd.user.services.unclutter = {
    Unit = {
      Description = "Unclutter hides lazy mouse cursor";
      After = ["graphical-session-pre.target"];
      PartOf = ["x11-session.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.unclutter-xfixes}/bin/unclutter";
      Restart = "always";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["x11-session.target"];
    };
  };
}
