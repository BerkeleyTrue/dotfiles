{pkgs, ...}: let
  rc = pkgs.writeShellScript "xplugrc" ''

    if [ "$1" == "display" ]; then
      # this is more annoying than useful
      # I'll just use it manually
      # systemd --user restart autorandr.service
      exit 0
    fi

    case "$1,$3,$4" in
    keyboard,*,*)
      systemctl --user restart xkbmap.service
      ;;
    esac
    exit 0
  '';
in {
  systemd.user.services.xplugd = {
    Unit = {
      Description = "X11 Plug Detection Daemon";
      Documentation = ["man:xplugd(1)"];
      After = ["x11-foundation.target"];
      PartOf = ["input-services.target"];
    };

    Service = {
      Type = "forking";
      ExecStart = "${pkgs.xplugd}/bin/xplugd ${rc}";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["input-services.target"];
    };
  };
}
