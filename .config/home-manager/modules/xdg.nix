{
  flake.modules.homeManager.xdg = {
    config,
    pkgs,
    ...
  }: let
    home = config.home.homeDirectory;
    dwns = config.xdg.userDirs.download;

    # some programs (fuck Zoom) ignore user-dirs and create ~/Downloads anyway
    # move whatever lands there into the real download dir and remove it
    downloads-cleanup = pkgs.writeShellApplication {
      name = "downloads-cleanup";
      runtimeInputs = with pkgs; [coreutils fd];
      text = ''
        src=${home}/Downloads
        dst=${dwns}
        [ -d "$src" ] && [ ! -L "$src" ] || exit 0
        mkdir -p "$dst"

        # --backup=numbered keeps both copies on name clashes
        # -H/-I so hidden and gitignored entries are moved too
        fd -H -I --max-depth 1 . "$src" -X mv --backup=numbered -t "$dst" || true

        # anything mv couldn't merge goes aside so the dir is always gone
        rmdir "$src" 2>/dev/null || mv "$src" "$dst/Downloads-$(date +%Y%m%d-%H%M%S)"
        echo "removed $src"
      '';
    };
  in {
    xdg.userDirs = {
      enable = true;
      setSessionVariables = true;
      desktop = "${home}/dsk";
      documents = "${home}/docs";
      download = "${home}/dwns";
      music = home;
      pictures = "${home}/pics";
      projects = null;
      publicShare = "${home}/pub";
      templates = "${home}/templates";
      videos = "${home}/vids";
    };

    systemd.user.services.downloads-cleanup = {
      Unit = {
        Description = "Remove ~/Downloads, moving its contents to the XDG download dir";
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${downloads-cleanup}/bin/downloads-cleanup";
      };
    };

    systemd.user.paths.downloads-cleanup = {
      Unit = {
        Description = "Watch for ~/Downloads being created";
      };

      Path = {
        PathExists = "${home}/Downloads";
      };

      Install = {
        WantedBy = ["paths.target"];
      };
    };

    systemd.user.services.xdg-desktop-portal-kde = {
      Unit = {
        Description = "XDG Desktop Portal for KDE";
        Documentation = ["man:xdg-desktop-portal(1)"];
        After = ["wayland-environment.service"];
        Before = ["niri.target"];
        PartOf = ["desktop-services.target"];
      };

      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.impl.portal.desktop.kde";
        ExecStart = "/usr/lib/xdg-desktop-portal-kde";
        Restart = "on-failure";
        RestartSec = 3;
      };

      Install = {
        WantedBy = ["desktop-services.target"];
      };
    };

    # needed to load env vars for systemd user services
    xdg.configFile."systemd/user.conf".text = ''
      [Manager]
      ManagerEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
      DefaultEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
    '';
  };
}
