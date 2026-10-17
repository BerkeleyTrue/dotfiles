{
  # Claude Code rewrites the claude-cli scheme handler in mimeapps.list.
  # Watch the file and unset the handler with handlr whenever it reappears.
  flake.modules.homeManager.mimeapps-guard = {
    config,
    pkgs,
    ...
  }: let
    file = "${config.xdg.configHome}/mimeapps.list";
    mime = "x-scheme-handler/claude-cli";
    guard = pkgs.writeShellApplication {
      name = "mimeapps-guard";
      runtimeInputs = [pkgs.handlr];
      text = ''
        # handlr get fails when no handler is set, so this is a no-op
        # after our own unset retriggers the path unit
        handlr get ${mime} >/dev/null 2>&1 || exit 0

        handlr unset ${mime}
        echo "unset ${mime}"
      '';
    };
  in {
    systemd.user.services.mimeapps-guard = {
      Unit = {
        Description = "Unset claude-cli scheme handler in mimeapps.list";
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${guard}/bin/mimeapps-guard";
      };

      Install = {
        WantedBy = ["default.target"];
      };
    };

    systemd.user.paths.mimeapps-guard = {
      Unit = {
        Description = "Watch mimeapps.list for changes";
      };

      Path = {
        PathChanged = file;
      };

      Install = {
        WantedBy = ["paths.target"];
      };
    };
  };
}
