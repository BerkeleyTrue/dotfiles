{...}: {
  systemd.user.targets.desktop-services = {
    Unit = {
      Description = "Desktop Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["niri.target"];
      Wants = [
        "notification.target"
        "tray.target"
      ];
      PartOf = ["niri-session.target"];
    };
  };

  systemd.user.targets.notification = {
    Unit = {
      Description = "Notification Services";
      Documentation = ["man:systemd.special(7)"];
      PartOf = ["desktop-services.target"];
    };
  };
}
