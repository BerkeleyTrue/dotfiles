{
  pkgs,
  lib,
  ...
}: let
  getExe = lib.getExe;
  swaync = "${pkgs.swaync}/bin/swaync-client";

  watch-sleep = pkgs.writeShellScriptBin "watch-sleep" ''
    dbus-monitor --system "type='signal', interface='org.freedesktop.login1.Manager', member=PrepareForSleep" | while read x; do
        case "$x" in
            *"boolean false"*) systemctl --user --no-block stop sleep.target;;
            *"boolean true"*) systemctl --user --no-block start sleep.target;;
        esac
    done
  '';

  pre-sleep = pkgs.writeShellScriptBin "pre-sleep" ''
    ${getExe pkgs.playerctl} pause &> /dev/null # exits non-zero if nothing is playing
    ${swaync} --dnd-on
  '';

  post-sleep = pkgs.writeShellScriptBin "post-sleep" ''
    echo "unlocking screen"
    ${swaync} --dnd-off
    makoify -a "Hephaestus" -u low -i distributor-logo-nixos "Welcome Back!"
  '';
in {
  home.packages = with pkgs; [
    playerctl
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
      ExecStart = "${getExe watch-sleep}";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = ["x11-session.target"];
    };
  };

  # home-manager xsecurelock won't unlock, use system package
  systemd.user.services.sleep = {
    Unit = {
      Description = "User suspend actions";
      Before = ["sleep.target"];
    };

    Service = {
      Type = "forking";
      Environment = "DISPLAY=:0";
      ExecStartPre = "${getExe pre-sleep}";
      ExecStart = "/usr/bin/xsecurelock";
      ExecStop = "${getExe post-sleep}";
    };

    Install = {
      WantedBy = ["sleep.target"];
    };
  };

  # TODO: is this needed for wayland unlock?
  # systemd.user.services.watch-lid = {
  #   Unit = {
  #     Description = "Watch for lid open events and move mouse to trigger unlock screen";
  #     After = ["graphical-session.target"];
  #   };
  #
  #   Service = {
  #     ExecStart = "${getExe watch-lid}";
  #     Restart = "on-failure";
  #   };
  #
  #   Install = {
  #     WantedBy = ["x11-session.target"];
  #   };
  # };
}
