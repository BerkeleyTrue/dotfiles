{pkgs, ...}: {
  systemd.user.services.xcape = {
    Unit = {
      Description = "XCape";
      After = ["graphical-session-pre.target" "xkbmap.service"];
      PartOf = ["x11-session.target"];
    };

    Service = {
      Type = "forking";
      ExecStart = "${pkgs.xcape}/bin/xcape -e 'Control_L=Escape'";
    };

    Install = {
      WantedBy = ["x11-session.target"];
    };
  };
}
