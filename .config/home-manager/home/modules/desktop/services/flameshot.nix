{pkgs, ...}: {
  home.packages = [
    pkgs.flameshot
  ];

  systemd.user.services.flameshot = {
    Unit = {
      Description = "Flameshot screenshot tool";
      Documentation = ["man:flameshot(1)"];
      After = ["tray.target" "graphical-session.target"];
      PartOf = ["tray.target"];
    };

    Service = {
      ExecStart = "${pkgs.flameshot}/bin/flameshot";
      Restart = "on-abort";
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateUsers = true;
      RestrictNamespaces = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = "@system-service";
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };
}
