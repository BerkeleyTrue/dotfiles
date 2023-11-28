{pkgs, ...}: {
  systemd.user.services.xkbmap = {
    Unit = {
      Description = "Set up keyboard in X";
      After = "graphical-session-pre.target";
      PartOf = ["graphical-session.target"];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
