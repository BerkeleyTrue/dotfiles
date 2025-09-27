{...}: {
  systemd.user.targets.desktop-services = {
    Unit = {
      Description = "Desktop Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["xmonad.target"];
      Wants = [
        "compositor.target"
        "notification.target" 
        "wallpaper.target"
        "tray.target"
        "desktop-utilities.target"
      ];
      PartOf = ["xmonad-session.target"];
    };
  };

  systemd.user.targets.compositor = {
    Unit = {
      Description = "Compositor Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["xmonad.target"];
      PartOf = ["desktop-services.target"];
    };
  };

  systemd.user.targets.notification = {
    Unit = {
      Description = "Notification Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["compositor.target"];
      PartOf = ["desktop-services.target"];
    };
  };

  systemd.user.targets.wallpaper = {
    Unit = {
      Description = "Wallpaper Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["compositor.target"];
      PartOf = ["desktop-services.target"];
    };
  };

  # home-manager doesn't allow overwriting xsesion.nix trayTarget
  # but we can add dependencies to it
  # built in.
  # Unit = {
  #   Description = "Home Manager System Tray";
  #   Requires = [ "graphical-session-pre.target" ];
  # };
  systemd.user.targets.tray = {
    Unit = {
      Documentation = "man:systemd.special(7)";
      After = ["notification.target"];
      PartOf = ["desktop-services.target"];
    };
  };

  systemd.user.targets.desktop-utilities = {
    Unit = {
      Description = "Desktop Utility Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["xmonad.target"];
      PartOf = ["desktop-services.target"];
    };
  };

  systemd.user.targets.input-services = {
    Unit = {
      Description = "Input Enhancement Services";
      Documentation = ["man:systemd.special(7)"];
      After = ["x11-foundation.target"];
      Before = ["xmonad.target"];
      PartOf = ["desktop-services.target"];
    };
  };
}
