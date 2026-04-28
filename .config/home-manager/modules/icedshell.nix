{
  inputs,
  lib,
  ...
}: {
  flake.modules.homeManager.icedshell = {
    pkgs,
    config,
    ...
  }: {
    options.programs.icedshell.package = lib.mkOption {
      type = lib.types.package;
      default = config.lib.nixGL.wrap pkgs.icedshell;
      readOnly = true;
      description = "The nixGL-wrapped icedshell package";
    };

    config = {
      nixpkgs.overlays = [
        inputs.icedshell.overlays.default
      ];

      home.packages = [
        config.programs.icedshell.package
      ];

      systemd.user.services.icedshell = {
        Unit = {
          Description = "Icedshell Bar";
          PartOf = ["tray.target"];
          After = ["graphical-session.target"];
          BindsTo = ["graphical-session.target"];
        };

        Service = {
          ExecStart = "${config.programs.icedshell.package}/bin/icedshell -v daemon";
          ExecReload = "${pkgs.coreutils}/bin/kill -SIGUSR2 $MAINPID";
          KillMode = "mixed";
          Restart = "on-failure";
        };

        Install = {
          WantedBy = ["tray.target"];
        };
      };
    };
  };
}
