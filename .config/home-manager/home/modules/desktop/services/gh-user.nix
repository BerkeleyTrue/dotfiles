{...}: {
  systemd.user.services.gh-user-sync = {
    Unit = {
      Description = "Run GH stat";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "%h/.local/bin/ghstat";
    };
  };

  systemd.user.timers.gh-user-sync = {
    Unit = {
      Description = "Run GH stat Timer";
    };

    Timer = {
      OnBootSec = 80;

      # run every 30 minutes
      OnCalendar = "*:0/10";
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };
}
