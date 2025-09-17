{
  pkgs,
  config,
  ...
}: {
  systemd.user.services.picom = {
    Unit = {
      Description = "Picom X11 compositor";
      After = ["graphical-session-pre.target"];
      PartOf = ["x11-session.target"];
    };

    Service = {
      ExecStart = "${config.lib.nixGL.wrap pkgs.picom}/bin/picom ";
      # ExecStart = "/usr/bin/picom ";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["x11-session.target"];
    };
  };
}
