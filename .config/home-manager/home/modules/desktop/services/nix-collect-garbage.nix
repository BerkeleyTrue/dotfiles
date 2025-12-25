{pkgs, ...}: {
  systemd.user.services.nix-collect-garbage = {
    Unit = {
      Description = "Collect Nix Garbage";
    };

    Service = {
      Type = "oneshot";
      ExecStart = let
        script = pkgs.writeShellApplication {
          name = "collect-garbage";
          text = ''
            #!/usr/bin/env bash
            /usr/bin/nix-collect-garbage --delete-older-than 10d
            makoify -a "Hephaestus" "Nix Garbage Collected" -i distributor-logo-nixos
          '';
        };
      in "${script}/bin/collect-garbage";
    };
  };

  systemd.user.timers.nix-collect-garbage = {
    Unit = {
      Description = "Run Nix Garbage Collector";
    };

    Timer = {
      # run every Saturday at 8pm
      OnCalendar = "Sat 20:00:00";
      Persistent = true;
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };
}
