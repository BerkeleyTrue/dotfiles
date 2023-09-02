{pkgs, ...}: {
  systemd.user.services.unclutter = {
    Unit = {
      Description = "Unclutter hides lazy mouse cursor";
      After = ["graphical-session-pre.target"];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.unclutter-xfixes}/bin/unclutter";
      Restart = "always";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
