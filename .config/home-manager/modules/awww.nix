{lib, ...}: {
  flake.modules.homeManager.desktop = {
    pkgs,
    config,
    ...
  }: let
    monitors = config.monitors;
    awww = pkgs.awww;
    outputs =
      lib.concatMapAttrs (name: spec: {"${name}" = spec.label;}) monitors;
    wallpaperScript = pkgs.writeShellApplication {
      name = "wallpaperScript";
      runtimeInputs = [
        awww
      ];
      text = ''
        awww restore
        ${lib.foldlAttrs (acc: name: output: "${acc}awww img --outputs ${output} ${config.home.nix-wallpapers.${name}.outputPath}\n") "" outputs}
      '';
    };
  in {
    packages = [awww];

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

    systemd.user.services.set-wallpaper = {
      Unit = {
        Description = "Set wallpaper";
        After = ["awww.service"];
        Requires = ["awww.service"];
      };

      Service = {
        Type = "oneshot";
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 2";
        ExecStart = "${lib.getExe wallpaperScript}";
      };

      Install = {WantedBy = ["awww.service"];};
    };
  };
}
