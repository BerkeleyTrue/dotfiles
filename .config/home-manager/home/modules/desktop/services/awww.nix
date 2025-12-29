{
  lib,
  pkgs,
  config,
  theme,
  hardware,
  ...
}: let
  awww = pkgs.awww;
  outputs =
    lib.concatMapAttrs (name: spec: {"${name}" = spec.label;}) hardware.monitors;

  wallpaperScript = pkgs.writeShellApplication {
    name = "wallpaperScript";
    runtimeInputs = [
      awww
    ];
    text = ''
      awww restore
      ${lib.foldlAttrs (acc: name: output: "awww img --outputs ${output} ${config.home.nix-wallpapers.${name}.outputPath}\n") "" outputs}
    '';
  };
in {
  home.nix-wallpapers = {
    g5 = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.mauve;
        endColor = theme.colors.sapphire;
      };
      colors = {
        color0 = theme.colors.overlay0;
        color1 = theme.colors.overlay0;
        color2 = theme.colors.overlay0;
        color3 = theme.colors.overlay0;
        color4 = theme.colors.overlay0;
        color5 = theme.colors.overlay0;
      };
      height = 1440;
      width = 3440;
    };

    dell = {
      angle = 30;
      gradient = {
        beginColor = theme.colors.lavender;
        endColor = theme.colors.sapphire;
      };
      colors = {
        color0 = theme.colors.subtext1;
        color1 = theme.colors.subtext1;
        color2 = theme.colors.subtext1;
        color3 = theme.colors.subtext1;
        color4 = theme.colors.subtext1;
        color5 = theme.colors.subtext1;
      };
      height = 1080;
      width = 2560;
    };

    framework = {
      angle = 60;
      swirl = 360;
      gradient = {
        beginColor = theme.colors.sapphire;
        endColor = theme.colors.lavender;
      };
      colors = {
        color0 = theme.colors.text;
        color1 = theme.colors.text;
        color2 = theme.colors.text;
        color3 = theme.colors.text;
        color4 = theme.colors.text;
        color5 = theme.colors.text;
      };
      height = 1504;
      width = 2256;
    };
  };
  home.packages = [awww];

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

    Install = {WantedBy = ["graphical-session.target"];};
  };
}
