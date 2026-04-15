{
  flake.modules.homeManager.gh = {pkgs, ...}: {
    home.packages = [
      pkgs.gh # GitHub CLI
    ];

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

        # run every 60 minutes
        OnCalendar = "*:0/60";
      };

      Install = {
        WantedBy = ["timers.target"];
      };
    };
  };
}
