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
      systemd --user restart xkbmap.service
      ;;
    esac
    exit 0
  '';
in {
  systemd.user.services.xplugd = {
    Unit = {
      Description = "Rerun after IO has changed using xplugd";
      After = ["graphical-session-pre.target"];
      PartOf = "graphical-session.target";
    };

    Service = {
      Type = "forking";
      ExecStart = "${pkgs.xplugd}/bin/xplugd ${rc}";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
