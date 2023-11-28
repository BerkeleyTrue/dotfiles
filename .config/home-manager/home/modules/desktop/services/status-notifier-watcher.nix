{pkgs, ...}: {
  home.packages = with pkgs; [
    haskellPackages.status-notifier-item # sni system tray protocol
  ];

  systemd.user.services.status-notifier-watcher = {
    Unit = {
      Description = "Status Notifier Item watcher";
      PartOf = ["tray.target"];
      Before = ["taffybar.service"];
    };

    Service = {
      Type = "dbus";
      BusName = "org.kde.StatusNotifierWatcher";
      ExecStart = "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
      Restart = "always";
    };

    Install = {
      WantedBy = ["tray.target" "taffybar.service"];
    };
  };
}
