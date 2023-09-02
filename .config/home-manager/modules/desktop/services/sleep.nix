{pkgs, ...}: let
  watch-sleep = pkgs.writeScriptBin "watch-sleep" ''
    #!/bin/bash
    dbus-monitor --system "type='signal',interface='org.freedesktop.login1.Manager',member=PrepareForSleep" | while read x; do
        case "$x" in
            *"boolean false"*) systemctl --user --no-block stop sleep.target;;
            *"boolean true"*) systemctl --user --no-block start sleep.target;;
        esac
    done
  '';
in {
  home.packages = with pkgs; [
    playerctl
    xsecurelock
    dunst
    watch-sleep
  ];

  systemd.user.targets.sleep = {
    Unit = {
      Description = "User level sleep target";
      StopWhenUnneeded = "yes";
    };
  };

  systemd.user.services.watch-sleep = {
    Unit = {
      Description = "Watch for sleep events and start/stop sleep.target";
      After = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "${watch-sleep}/bin/watch-sleep";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };

  # xsecurelock wouldn't unlock
  # systemd.user.services.sleep = {
  #   Unit = {
  #     Description = "User suspend actions";
  #     Before = [ "sleep.target" ];
  #   };
  #
  #   Service = {
  #     Type = "forking";
  #     Environment = "DISPLAY=:0";
  #     ExecStartPre = "${pkgs.playerctl}/bin/playerctl pause & ${pkgs.dunst}/bin/dunstctl set-paused true";
  #     ExecStart = "${pkgs.xsecurelock}/bin/xsecurelock";
  #     ExecStop = "${pkgs.dunst}/bin/dunstctl set-paused false";
  #   };
  #
  #   Install = {
  #     WantedBy = [ "sleep.target" ];
  #   };
  # };
}
