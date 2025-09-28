{...}: {
  systemd.user.services.taffybar = {
    Unit = {
      Description = "Taffybar Status Bar";
      PartOf = ["tray.target"];
      After = ["xmonad.target"];
      BindsTo = ["x11-session.target"];
      ConditionEnvironment = "XDG_SESSION_TYPE=x11";
    };

    Service = {
      Type = "dbus";
      BusName = "org.taffybar.Bar";
      ExecStart = "%h/.local/bin/taffybar";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };
}
