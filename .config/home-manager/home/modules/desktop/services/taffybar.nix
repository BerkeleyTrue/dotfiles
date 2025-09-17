{...}: {
  systemd.user.services.taffybar = {
    Unit = {
      Description = "Taffybar Status Bar";
      PartOf = ["tray.target"];
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