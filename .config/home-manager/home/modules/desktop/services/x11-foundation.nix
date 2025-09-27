{
  pkgs,
  config,
  ...
}: let
  X11_HOME = "${config.xdg.configHome}/x11";
in {
  # groups essential X11 setup services
  systemd.user.targets.x11-foundation = {
    Unit = {
      Description = "X11 Foundation Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["x11-environment.service"];
      Wants = [
        "xresources.service"
        "xmodmap.service" 
        "xkbmap.service"
        "xbindkeys.service"
      ];
      PartOf = ["x11-session.target"];
    };
  };

  systemd.user.services.xresources = {
    Unit = {
      Description = "Load X Resources";
      Documentation = ["man:xrdb(1)"];
      After = ["x11-environment.service"];
      Before = ["x11-foundation.target"];
      PartOf = ["x11-foundation.target"];
      ConditionPathExists = "${X11_HOME}/xresources";
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.xorg.xrdb}/bin/xrdb -merge ${X11_HOME}/xresources";
    };

    Install = {
      WantedBy = ["x11-foundation.target"];
    };
  };

  systemd.user.services.xmodmap = {
    Unit = {
      Description = "Apply X Modifier Map";
      Documentation = ["man:xmodmap(1)"];
      After = ["xresources.service"];
      Before = ["x11-foundation.target"];
      PartOf = ["x11-foundation.target"];
      ConditionPathExists = "${X11_HOME}/xmodmap";
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.xorg.xmodmap}/bin/xmodmap ${X11_HOME}/xmodmap";
    };

    Install = {
      WantedBy = ["x11-foundation.target"];
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
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
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
      After = ["xmodmap.service"];
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
