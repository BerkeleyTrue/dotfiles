{pkgs, ...}: {
  home.packages = [
    pkgs.flameshot
  ];

  systemd.user.services.flameshot = {
    Unit = {
      Description = "Flameshot screenshot tool";
      Requires = "tray.target";
      After = [
        "graphical-session-pre.target"
        "tray.target"
      ];
      PartOf = ["graphical-session.target"];
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
      WantedBy = ["graphical-session.target"];
    };
  };
}
