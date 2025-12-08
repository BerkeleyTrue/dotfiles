{pkgs, ...}: {
  systemd.user.services.waybar = {
    Unit = {
      Description = "Waybar Status Bar";
      PartOf = ["tray.target"];
      Before = ["niri-session.target"];
      BindsTo = ["niri-session.target"];
    };

    Service = {
      ExecStart = "${pkgs.waybar}/bin/waybar";
      ExecReload = "${pkgs.coreutils}/bin/kill -SIGUSR2 $MAINPID";
      KillMode = "mixed";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };
}
