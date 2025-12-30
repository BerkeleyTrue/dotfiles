{
  pkgs,
  config,
  ...
}: {
  systemd.user.services.waybar = {
    Unit = {
      Description = "Waybar Status Bar";
      PartOf = ["tray.target"];
      After = ["graphical-session.target"];
      BindsTo = ["graphical-session.target"];
      X-Reload-Triggers =
        "${config.xdg.configFile."waybar/config".source}" ++ "${config.xdg.configFile."waybar/style.css".source}";
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
