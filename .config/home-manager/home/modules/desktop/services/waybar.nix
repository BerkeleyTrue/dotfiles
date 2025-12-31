{
  pkgs,
  ...
}: {
  systemd.user.services.waybar = {
    Unit = {
      Description = "Waybar Status Bar";
      PartOf = ["tray.target"];
      After = ["graphical-session.target"];
      BindsTo = ["graphical-session.target"];
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
