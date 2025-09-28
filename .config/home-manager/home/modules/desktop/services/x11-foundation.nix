{pkgs, ...}: {
  # groups essential X11 setup services
  systemd.user.targets.x11-foundation = {
    Unit = {
      Description = "X11 Foundation Services";
      Documentation = ["man:systemd.special(7)"];
      Wants = [
        "xkbmap.service"
        "xbindkeys.service"
      ];
      PartOf = ["x11-session.target"];
    };
  };

  systemd.user.services.xbindkeys = {
    Unit = {
      Description = "X11 Hotkey Daemon";
      Documentation = ["man:xbindkeys(1)"];
      After = ["xkbmap.service"];
      Before = ["x11-foundation.target"];
      PartOf = ["x11-foundation.target"];
      ConditionPathExists = "%h/.xbindkeysrc";
    };

    Service = {
      Type = "forking";
      ExecStart = "${pkgs.xbindkeys}/bin/xbindkeys";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["x11-foundation.target"];
    };
  };

  systemd.user.services.xkbmap = {
    Unit = {
      Description = "Set up keyboard in X";
      Documentation = ["man:setxkbmap(1)"];
      Before = ["x11-foundation.target"];
      PartOf = ["x11-foundation.target"];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps";
    };

    Install = {
      WantedBy = ["x11-foundation.target"];
    };
  };
}
