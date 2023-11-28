{...}: {
  systemd.user.services.notes-sync = {
    Unit = {
      Description = "Run NotesSync";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "%h/.local/bin/notessync";
    };
  };

  systemd.user.timers.notes-sync = {
    Unit = {
      Description = "Notes Sync Timer";
    };

    Timer = {
      OnBootSec = 80;

      # run every 30 minutes
      OnCalendar = "*:0/30";
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };
}
