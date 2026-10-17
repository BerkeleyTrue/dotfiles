{
  # Claude Code rewrites the claude-cli scheme handler in mimeapps.list.
  # Watch the file and pin the handler back to `true` whenever it changes.
  flake.modules.homeManager.mimeapps-guard = {
    config,
    pkgs,
    ...
  }: let
    file = "${config.xdg.configHome}/mimeapps.list";
    want = "x-scheme-handler/claude-cli=true;";
    guard = pkgs.writeShellApplication {
      name = "mimeapps-guard";
      runtimeInputs = with pkgs; [coreutils gnugrep gawk];
      text = ''
        f=${file}
        want='${want}'
        [ -f "$f" ] || exit 0

        # already correct: exactly one claude-cli line and it's ours
        if [ "$(grep -c '^x-scheme-handler/claude-cli=' "$f")" = 1 ] && grep -qxF "$want" "$f"; then
          exit 0
        fi

        tmp=$(mktemp)
        trap 'rm -f "$tmp"' EXIT
        awk -v want="$want" '
          /^x-scheme-handler\/claude-cli=/ { next }
          { print }
          $0 == "[Default Applications]" { print want; done = 1 }
          END { if (!done) { print ""; print "[Default Applications]"; print want } }
        ' "$f" > "$tmp"

        # write in place to keep the file's inode and permissions
        cat "$tmp" > "$f"
        echo "restored $want in $f"
      '';
    };
  in {
    systemd.user.services.mimeapps-guard = {
      Unit = {
        Description = "Keep claude-cli scheme handler pinned in mimeapps.list";
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
