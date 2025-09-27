{pkgs, ...}: {
  systemd.user.services.unclutter = {
    Unit = {
      Description = "Unclutter Cursor Manager";
      Documentation = ["man:unclutter(1)"];
      After = ["xmonad.target"];
      PartOf = ["desktop-utilities.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.unclutter-xfixes}/bin/unclutter";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["desktop-utilities.target"];
    };
  };
}
