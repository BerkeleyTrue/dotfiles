{pkgs, ...}: {
  systemd.user.services.xcape = {
    Unit = {
      Description = "XCape Key Remapping";
      Documentation = ["man:xcape(1)"];
      After = ["x11-foundation.target"];
      PartOf = ["input-services.target"];
    };

    Service = {
      Type = "forking";
      ExecStart = "${pkgs.xcape}/bin/xcape -e 'Control_L=Escape'";
      Restart = "on-failure";
      RestartSec = 3;
      TimeoutStartSec = "10s";
    };

    Install = {
      WantedBy = ["input-services.target"];
    };
  };
}
