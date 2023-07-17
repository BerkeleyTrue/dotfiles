{ pkgs, ... }:
{
  # A terminal multiplexer
  home.packages = [ pkgs.tmux ];

  systemd.user.services.tmux = {
    Unit = {
      Description = "Tmux server";
      After = [ "graphical-session-pre.target" "tray.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.tmux}/bin/tmux new-session - d - s dev";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
