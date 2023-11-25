{pkgs, ...}: let
  watch-sleep = pkgs.writeShellScriptBin "watch-sleep" ''
    dbus-monitor --system "type='signal', interface='org.freedesktop.login1.Manager', member=PrepareForSleep" | while read x; do
        case "$x" in
            *"boolean false"*) systemctl --user --no-block stop sleep.target;;
            *"boolean true"*) systemctl --user --no-block start sleep.target;;
        esac
    done
  '';

  pre-sleep = pkgs.writeShellScriptBin "pre-sleep" ''
    ${pkgs.playerctl}/bin/playerctl pause &> /dev/null # exits non-zero if nothing is playing
    ${pkgs.dunst}/bin/dunstctl set-paused true
  '';

  post-sleep = pkgs.writeShellScriptBin "post-sleep" ''
    echo "unlocking screen"
    ${pkgs.dunst}/bin/dunstify -a "Hephaestus" -u low -i distributor-logo-nix "Welcome Back!"
    ${pkgs.dunst}/bin/dunstctl set-paused false
  '';
in {
  home.packages = with pkgs; [
    playerctl
    dunst
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

  # home-manager xsecurelock won't unlock, use system package
  systemd.user.services.sleep = {
    Unit = {
      Description = "User suspend actions";
      Before = ["sleep.target"];
    };

    Service = {
      Type = "forking";
      Environment = "DISPLAY=:0";
      ExecStartPre = "${pre-sleep}/bin/pre-sleep";
      ExecStart = "/usr/bin/xsecurelock";
      ExecStop = "${post-sleep}/bin/post-sleep";
    };

    Install = {
      WantedBy = ["sleep.target"];
    };
  };
}
