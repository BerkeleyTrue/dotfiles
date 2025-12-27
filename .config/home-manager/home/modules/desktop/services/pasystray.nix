# doesn't work in wayland https://github.com/christophgysin/pasystray/issues/90
{pkgs, ...}: {
  systemd.user.services.pasystray = {
    Unit = {
      Description = "PulseAudio System Tray";
      Documentation = ["man:pasystray(1)"];
      After = ["tray.target"];
      PartOf = ["tray.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.pasystray}/bin/pasystray";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };
}
