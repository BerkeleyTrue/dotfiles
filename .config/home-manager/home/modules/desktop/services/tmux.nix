# ref: https://superuser.com/questions/1581577/running-two-tmux-sessions-as-systemd-service/1582196#1582196
{
  pkgs,
  lib,
  ...
}:
with lib; let
  pluginName = p:
    if types.package.check p
    then p.pname
    else p.plugin.pname;
  tmuxPlugins = with pkgs.tmuxPlugins; [
    battery
    better-mouse-mode
    copycat
    online-status
    prefix-highlight
    vim-tmux-navigator
    yank
  ];

  tmuxPluginsConf = pkgs.writeText "tmux-plugins.tmux" ''
    # ============================================= #
    # Load plugins Managed by Home-Manager          #
    # --------------------------------------------- #
    ${(concatMapStringsSep "\n" (p: ''
        # ${pluginName p}
        # ---------------------
        run-shell ${
          if types.package.check p
          then p.rtp
          else p.plugin.rtp
        }
      '')
      tmuxPlugins)}
    # ============================================= #
  '';
in {
  # A terminal multiplexer
  home.packages =
    [
      pkgs.tmux
    ]
    ++ tmuxPlugins;

  xdg.configFile."tmux/tmux-plugins.tmux".source = tmuxPluginsConf;

  # master session will be started empty so it will always fork
  # if a session already exists before this, the command will exit
  # triggering ExecStop
  systemd.user.services.tmux-server = {
    Unit = {
      Description = "Tmux Server";
    };

    Service = {
      Type = "forking";
      # requires "set-option -g exit-empty off" in tmux.conf
      # in order to fork when no sessions exist
      ExecStart = "${pkgs.tmux}/bin/tmux start-server";
      ExecStop = "${pkgs.tmux}/bin/tmux kill-server";
    };
  };

  systemd.user.services.tmux = {
    Unit = {
      Description = "Tmux session";
      PartOf = ["tmux-server.service"];
      After = ["graphical-session-pre.target"];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.tmux}/bin/tmux new-session -s dev -d";
      ExecStop = "${pkgs.tmux}/bin/tmux kill-session -t dev";
    };
  };
}
