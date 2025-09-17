{pkgs, ...}: {
  systemd.user.services.xkbmap = {
    Unit = {
      Description = "Set up keyboard in X";
      After = "graphical-session-pre.target";
      PartOf = ["x11-session.target"];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps";
    };

    Install = {
      WantedBy = ["x11-session.target"];
    };
  };
}
