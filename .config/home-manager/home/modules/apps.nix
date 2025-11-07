{
  pkgs,
  theme,
  config,
  ...
}: let
  rofi = pkgs.rofi.override {
    plugins = with pkgs; [
      rofi-calc
      rofi-emoji
    ];
  };

  myAspell = pkgs.aspellWithDicts (dicts: with dicts; [en en-computers en-science]);

  enDict = pkgs.runCommand "aspell-english-dict" {} ''
    mkdir -p $out/share/aspell
    ${myAspell}/bin/aspell -d en dump master | ${myAspell}/bin/aspell -l en expand > $out/share/aspell/english
  '';

  rofi-spell = pkgs.writeShellScriptBin "rofi-spell" ''
    word=$(cat ${enDict}/share/aspell/english | rofi -p 'spell' -dmenu)
    definition=$(${pkgs.wordnet}/bin/wn $word -over)

    if [[ ! -z "$definition" ]]; then
      rofi -p -e "$definition"
    fi

    echo -n $word | ${pkgs.nodePackages.clipboard-cli}/bin/clipboard
    echo $word
  '';

  rofi-usb = pkgs.writeShellApplication {
    name = "rofi-usb";
    runtimeInputs = [
      pkgs.rofi
      pkgs.udiskie
    ];
    text = ''
      device=$(udiskie-info --all --output "{ui_label}" | rofi -p 'usb' -dmenu | cut -d':' -f1)

      if [ -n "$device" ] ; then
        if mount | grep "$device" ; then
          echo "unmounting"
          udisksctl unmount -b "$device"
        else
          echo "mounting"
          udisksctl mount -b "$device"
        fi
      fi
    '';
  };
in {
  home.packages =
    (with pkgs; [
      # cutecom # serial terminal
      freecad # 3D CAD modeler
      gparted # graphical partition manager
      gimp3 # GNU Image Manipulation Program
      inkscape # vector graphics editor
      # krita # digital painting software # doesn't run
      keybase # encrypted chat
      keybase-gui # encrypted chat
      libation # an audible player/drm remover
      networkmanagerapplet # network manager applet
      # printrun # 3D printing host software - bad dependencies
      rofi-bluetooth # rofi bluetooth manager
      spacenavd # 3Dconnexion device driver
      viewnior # fast image preview
      vlc # Cross-platform media player and streaming server
      wordnet # lexical database for the English language
      zathura # pdf viewer
    ])
    ++ [
      (config.lib.nixGL.wrap pkgs.kitty) # GPU-accelerated terminal emulator
      # (config.lib.nixgl.wrapPackage pkgs.kicad)
      rofi # launcher
      rofi-spell # spell checker
      rofi-usb # rofi usb manager
      enDict # My dictionary
      myAspell # spell checker
    ];

  # don't know why this stopped working
  # xdg.dataFile."/aspell/english".source = "${enDict}/share/aspell/english";
  home.file.".local/share/aspell/english".source = "${enDict}/share/aspell/english";

  # enable app icons in the system tray for udiskie and nm-applet hm services
  xsession.preferStatusNotifierItems = true;

  services.keybase = {
    enable = true;
  };

  services.kbfs = {
    enable = true;
    mountPoint = "docs/keybase";
  };

  services.udiskie = {
    enable = true;
    automount = false;
    notify = true;
    tray = "always";
  };

  systemd.user.services.udiskie.Unit = {
    PartOf = ["tray.target"];
  };

  services.network-manager-applet = {
    enable = true;
  };

  systemd.user.services.network-manager-applet.Unit = {
    PartOf = ["tray.target"];
  };

  programs.rofi-network-manager = {
    enable = true;
    settings = {
      CHANGE_BARS = true;
      ASCII_OUT = true;
    };
    theme = let
      inherit (config.lib.formats.rasi) mkLiteral;
      inherit (config.lib.formats.rasi) mkRef;
      inherit (config.lib.formats.rasi) mkSimpleEl;
      c = builtins.mapAttrs (name: value: mkLiteral value) theme.colors;
      cls = theme.colors;
    in {
      configuration = {
        show-icons = false;
        sidebar-mode = false;
        hover-select = true;
        me-select-entry = "";
        me-accept-entry = [(mkLiteral "MousePrimary")];
      };

      "*" = {
        font = "FiraCode Nerd Font 18";
        foreground = c.text;
        background = c.base;

        background-color = mkRef "background";
        active-background = c.subtext1;

        urgent-background = c.red;
        urgent-foreground = mkRef "background";

        selected-background = mkRef "active-background";
        selected-urgent-background = mkRef "urgent-background";
        selected-active-background = mkRef "active-background";

        separatorcolor = mkRef "active-background";
        bordercolor = c.rosewater;
      };

      window = {
        text-color = mkRef "foreground";
        border-color = mkRef "bordercolor";
        border-radius = 6;
        border = 3;
        padding = 10;
      };
      mainbox = {
        border = 0;
        padding = 0;
      };
      textbox = {
        text-color = mkRef "foreground";
      };
      listview = {
        border = 0;
        dynamic = true;
        fixed-height = false;
        scrollbar = false;
        spacing = mkLiteral "4px";
        text-color = mkRef "separatorcolor";
        padding = mkLiteral "2px 0px 0px";
      };
      element = {
        border = 0;
        border-radius = mkLiteral "4px";
        padding = mkLiteral "8px 10px";
      };
      element-text = {
        background-color = mkLiteral "inherit";
        text-color = mkLiteral "inherit";
      };
      # normal rows
      "element.normal.normal" = mkSimpleEl (mkRef "background") (mkRef "foreground");
      "element.normal.urgent" = mkSimpleEl (mkRef "urgent-background") (mkRef "urgent-foreground");

      # table header is active
      "element.normal.active" = {
        background-color = mkLiteral "transparent";
        background-image = mkLiteral (with cls; "linear-gradient(40, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${text}, ${lavender})");
        text-color = mkLiteral cls.base;
      };
      # table header when selected
      "element.selected.active" = {
        background-color = mkLiteral "transparent";
        background-image = mkLiteral (with cls; "linear-gradient(40, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${subtext1}, ${text}, ${lavender})");
        text-color = mkLiteral cls.base;
      };

      "element.selected.normal" = {
        background-color = mkLiteral "transparent";
        background-image = mkLiteral (with cls; "linear-gradient(40, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${mauve}, ${sky})");
        text-color = mkRef "background";
      };
      "element.selected.urgent" = mkSimpleEl (mkRef "urgent-background") (mkRef "urgent-foreground");

      "element.alternate.normal" = mkSimpleEl (mkRef "background-color") (mkRef "foreground");
      "element.alternate.urgent" = mkSimpleEl (mkRef "background-color") (mkRef "urgent-foreground");
      "element.alternate.active" = mkSimpleEl (mkRef "active-background") (mkRef "foreground");

      mode-switcher = {
        border = 0;
      };

      "button selected" = {
        text-color = mkRef "foreground";
        background-color = mkRef "selected-background";
      };
      "button normal" = {
        text-color = mkRef "foreground";
      };

      inputbar = {
        text-color = mkRef "foreground";
        children = [
          (mkLiteral "textbox-prompt-colon")

          (mkLiteral "entry")
        ];
        padding = mkLiteral "1px";
      };

      textbox-prompt-colon = {
        expand = false;
        margin = 0;
        text-color = mkRef "foreground";
      };
      entry = {
        spacing = 0;
        text-color = mkRef "foreground";
        placeholder = "";
      };
    };
  };

  programs.terminator = {
    enable = true;
    config = {
      profiles.default.font = "FiraCode Nerd Font 18";
    };
  };

  programs.firefox = let
    lock-false = {
      Value = false;
      Status = "locked";
    };

    lock-true = {
      Value = true;
      Status = "locked";
    };

    userChrome = ''
      :root {
        font: 14px "FiraCode Nerd Font", monospace !important;
      }

      /* hides the native tabs */
      #TabsToolbar {
        visibility: collapse;
      }

      /* hides the sidebar header */
      #sidebar-header {
        visibility: collapse !important;
      }
    '';
  in {
    enable = true;

    package = config.lib.nixgl.wrapPackage pkgs.firefox;
    nativeMessagingHosts = [pkgs.fx-cast-bridge];

    # Check about:policies#documentation for options.
    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
      DisablePocket = true;
      DisableFirefoxAccounts = false;
      DisableAccounts = false;
      DisableFirefoxScreenshots = true;
      OverrideFirstRunPage = "";
      OverridePostUpdatePage = "";
      DontCheckDefaultBrowser = true;
      DisplayBookmarksToolbar = "never"; # alternatives: "always" or "newtab"
      DisplayMenuBar = "default-off"; # alternatives: "always", "never" or "default-on"
      SearchBar = "unified"; # alternative: "separate"
      DefaultDownloadDirectory = "\${home}/dwns";

      # Check about:config for options.
      Preferences = {
        "browser.contentblocking.category" = {
          Value = "strict";
          Status = "locked";
        };
        "extensions.pocket.enabled" = lock-false;
        "extensions.screenshots.disabled" = lock-true;
        "browser.topsites.contile.enabled" = lock-false;
        "browser.formfill.enable" = lock-false;
        "browser.search.suggest.enabled" = lock-false;
        "browser.search.suggest.enabled.private" = lock-false;
        "browser.urlbar.suggest.searches" = lock-false;
        "browser.tabs.tabmanager.enabled" = lock-false;
        "browser.urlbar.showSearchSuggestionsFirst" = lock-false;
        "browser.newtabpage.activity-stream.feeds.section.topstories" = lock-false;
        "browser.newtabpage.activity-stream.feeds.snippets" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includePocket" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = lock-false;
        "browser.newtabpage.activity-stream.section.highlights.includeVisited" = lock-false;
        "browser.newtabpage.activity-stream.showSponsored" = lock-false;
        "browser.newtabpage.activity-stream.system.showSponsored" = lock-false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = lock-false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = lock-true;
      };
    };
    profiles = {
      default = {
        inherit userChrome;
      };
    };
  };

  # General-purpose media player, fork of MPlayer and mplayer2
  programs.mpv = {
    enable = true;
    package = config.lib.nixGL.wrap pkgs.mpv;
    bindings = {
      "MBTN_LEFT" = "cycle pause";

      "WHEEL_RIGHT" = "seek 10";
      "WHEEL_LEFT" = "seek -10";
      "WHEEL_DOWN" = "add volume -2";
      "WHEEL_UP" = "add volume 2";

      "Ctrl+WHEEL_UP" = "add video-zoom 0.25";
      "Ctrl+WHEEL_DOWN" = "add video-zoom -0.25";

      "Alt+h" = "add video-pan-x -0.05";
      "Alt+j" = "add video-pan-y -0.05";
      "Alt+k" = "add video-pan-y 0.05";
      "Alt+l" = "add video-pan-x 0.05";

      "UP" = "add volume  2";
      "DOWN" = "add volume -2";

      "h" = "seek -10";
      "j" = "add volume -2";
      "k" = "add volume 2";
      "l" = "seek 10";
    };
    config = {
      loop-playlist = "inf";
    };
  };

  xdg.desktopEntries.rofi-bluetooth = {
    name = "Rofi Bluetooth Manager";
    genericName = "Bluetooth Manager";
    comment = "Bluetooth";
    icon = "bluetooth";
    exec = "rofi-bluetooth";
    terminal = false;
    categories = ["Network"];
  };

  xdg.desktopEntries.rofi-spell = {
    name = "Rofi Spell";
    genericName = "Dictionary";
    comment = "Dictionary";
    icon = "accessories-dictionary";
    exec = "${rofi-spell}/bin/rofi-spell";
    terminal = false;
    categories = ["Utility"];
  };

  xdg.desktopEntries.rofi-usb = {
    name = "Rofi Usb";
    genericName = "Usb";
    comment = "Usb";
    icon = "drive-harddisk-usb";
    exec = "${rofi-usb}/bin/rofi-usb";
    terminal = false;
    categories = ["Utility"];
  };

  xdg.desktopEntries.redis-insight = {
    name = "Redis Insight";
    genericName = "Redis Insight";
    comment = "Redis Insight";
    icon = "sqlitebrowser";
    exec = "redis-insight";
    terminal = false;
    categories = ["Utility"];
  };
}
