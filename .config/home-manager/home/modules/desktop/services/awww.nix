{
  lib,
  pkgs,
  ...
}: let
  awww = pkgs.awww;
in {
  home.packages = [ awww ];
  systemd.user.services.awww = {
    Unit = {
      Description = "awww wallpaper daemon";
      ConditionEnvironment = "WAYLAND_DISPLAY";
      After = ["graphical-session.target"];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "${lib.getExe' awww "awww-daemon"}";
      Environment = [
        "PATH=$PATH:${lib.makeBinPath [awww]}"
      ];
      Restart = "always";
      RestartSec = 10;
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
