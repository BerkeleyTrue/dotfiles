{
  pkgs,
  config,
  ...
}: {
  systemd.user.services.picom = {
    Unit = {
      Description = "Picom X11 compositor";
      Documentation = ["man:picom(1)"];
      After = ["xmonad.target"];
      PartOf = ["compositor.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${config.lib.nixGL.wrap pkgs.picom}/bin/picom";
      # ExecStart = "/usr/bin/picom";
      Restart = "on-failure";
      RestartSec = 3;
      TimeoutStartSec = "10s";
    };

    Install = {
      WantedBy = ["compositor.target"];
    };
  };
}
