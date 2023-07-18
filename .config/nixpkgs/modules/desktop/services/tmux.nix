# ref: https://superuser.com/questions/1581577/running-two-tmux-sessions-as-systemd-service/1582196#1582196
{ pkgs, ... }:
{
  # A terminal multiplexer
  home.packages = [ pkgs.tmux ];

  # master session will be started empty so it will always fork
  # if a session already exists before this, the command will exit
  # triggering ExecStop
  systemd.user.services.tmux-server = {
    Unit = {
      Description = "Tmux Server";
      After = [ "graphical-session-pre.target" "tray.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      Type = "forking";
      # requires "set-option -g exit-empty off" in tmux.conf
      # in order to fork when no sessions exist
      ExecStart = "${pkgs.tmux}/bin/tmux start-server";
      ExecStop = "${pkgs.tmux}/bin/tmux kill-server";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  systemd.user.services.tmux = {
    Unit = {
      Description = "Tmux session";
      PartOf = [ "tmux-server.service" ];
      After = [ "tmux-server.service" ];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.tmux}/bin/tmux new-session -s dev -d";
      ExecStop = "${pkgs.tmux}/bin/tmux kill-session -t dev";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
