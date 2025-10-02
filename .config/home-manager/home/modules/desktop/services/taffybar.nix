{...}: {
  systemd.user.services.taffybar = {
    Unit = {
      Description = "Taffybar Status Bar";
      PartOf = ["tray.target"];
      # we want taffybar to stop before x11-session so it cleanly shuts down.
      # We start x11-session with xmonad.target in xinit so X server should alread be running.
      # xmonad.target -> x11-session.target, which starts grapical-session.target, which starts tray.target
      Before = ["x11-session.target"]; # Start before X11 session stops
      BindsTo = ["x11-session.target"]; # Stop when X11 session stops
      # ConditionEnvironment = "XDG_SESSION_TYPE=x11";
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
