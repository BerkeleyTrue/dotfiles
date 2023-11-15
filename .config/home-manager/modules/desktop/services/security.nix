{...}: {
  systemd.user.services.polkit = {
    Unit = {
      Description = "KDE PolicyKit Authentication Agent";
      After = ["graphical-session.target"];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "/usr/lib/polkit-kde-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 15;
      Slice = "background.slice";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
