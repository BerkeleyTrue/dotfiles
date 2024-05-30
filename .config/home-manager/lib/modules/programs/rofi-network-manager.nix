{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.programs.rofi-network-manager;
  types = lib.types;
  mkOption = lib.mkOption;

  mkValueString = value:
    if lib.isBool value
    then
      if value
      then "true"
      else "false"
    else if lib.isInt value
    then toString value
    else if (value._type or "") == "literal"
    then value.value
    else if lib.isString value
    then ''"${value}"''
    else if lib.isList value
    then "[ ${lib.strings.concatStringsSep "," (map mkValueString value)} ]"
    else abort "Unhandled value type ${builtins.typeOf value}";

  mkKeyValue = {
    sep ? ": ",
    end ? ";",
  }: name: value: "  ${name}${sep}${mkValueString value}${end}";

  toKeyValue = lib.generators.toKeyValue {
    listsAsDuplicateKeys = true;
  };

  mkRasiSection = name: value:
    if lib.isAttrs value
    then let
      toRasiKeyValue = lib.generators.toKeyValue {mkKeyValue = mkKeyValue {};};
      # Remove null values so the resulting config does not have empty lines
      configStr = toRasiKeyValue (lib.filterAttrs (_: v: v != null) value);
    in ''
      ${name} {
      ${configStr}}
    ''
    else
      (mkKeyValue
        {
          sep = " ";
          end = "";
        }
        name
        value)
      + "\n";

  rasiLiteral =
    lib.types.submodule
    {
      options = {
        _type = mkOption {
          type = lib.types.enum ["literal"];
          internal = true;
        };

        value = mkOption {
          type = lib.types.str;
          internal = true;
        };
      };
    }
    // {
      description = "Rasi literal string";
    };

  primitive = types.oneOf [types.str types.int types.bool rasiLiteral];
  configType = types.attrsOf (types.either primitive (types.listOf primitive));
  themeType = types.either (types.attrsOf configType) types.bool;

  toRasi = attrs:
    lib.concatStringsSep "\n" (lib.concatMap (lib.mapAttrsToList mkRasiSection) [
      (lib.filterAttrs (n: _: n == "@theme") attrs)
      (lib.filterAttrs (n: _: n == "@import") attrs)
      (removeAttrs attrs ["@theme" "@import"])
    ]);
in {
  options.programs.rofi-network-manager = {
    enable = lib.mkEnableOption "rofi-network-manager: network manager in rofi";
    settings = mkOption {
      description = lib.mdDoc ''
        The settings for rofi-network-manager.
      '';
      type = types.submodule {
        options = {
          LOCATION = mkOption {
            description = ''
              The grid represents the screen with the numbers indicating the location of the window.
              If you want the window to be in the upper right corner, set location to 3.
            '';
            default = 0;
            type = types.int;
          };

          X_AXIS = mkOption {
            description = ''
              This sets the distance of the window from the edge of the screen on the X axis.
            '';
            default = 0;
            type = types.int;
          };

          Y_AXIS = mkOption {
            description = ''
              This sets the distance of the window from the edge of the screen on the Y axis.
            '';
            default = 0;
            type = types.int;
          };

          NOTIFICATION = mkOption {
            description = ''
              This sets whether a notification should be displayed when a connection is established.
            '';
            default = false;
            type = types.bool;
          };

          QRCODE_LOCATION = mkOption {
            description = ''
              This sets the anchor point for the window displaying the QR code.
            '';
            default = 0;
            type = types.int;
          };

          QRCODE_DIR = mkOption {
            description = ''
              This sets the directory where the QR code is saved.
            '';
            default = "/tmp/";
            type = types.str;
          };

          WIDTH_FIX_MAIN = mkOption {
            description = ''
              WIDTH_FIX_MAIN/WIDTH_FIX_STATUS

              These values can be adjusted if the text doesn't fit or
              if there is too much space at the end when you launch the script.
              It will depend on the font type and size.
            '';
            default = 7;
            type = types.int;
          };

          WIDTH_FIX_STATUS = mkOption {
            description = ''
              WIDTH_FIX_MAIN/WIDTH_FIX_STATUS

              These values can be adjusted if the text doesn't fit or
              if there is too much space at the end when you launch the script.
              It will depend on the font type and size.
            '';
            default = 10;
            type = types.int;
          };

          ASCII_OUT = mkOption {
            description = ''
              Set it to true, if the script outputs the signal strength with asterisks
              and you want  bars.
            '';
            default = false;
            type = types.bool;
          };

          CHANGE_BARS = mkOption {
            description = ''
              Set it to true if you want to use custom icons
              for the signal strength instead of the default ones.
            '';
            default = false;
            type = types.bool;
          };

          SIGNAL_STRENGTH_0 = mkOption {
            description = ''
              Set it to the icon you want to use for the signal strength 0.
            '';
            default = "󰤭 ";
            type = types.str;
          };

          SIGNAL_STRENGTH_1 = mkOption {
            description = ''
              Set it to the icon you want to use for the signal strength 1.
            '';
            default = "󰤟 ";
            type = types.str;
          };

          SIGNAL_STRENGTH_2 = mkOption {
            description = ''
              Set it to the icon you want to use for the signal strength 2.
            '';
            default = "󰤢 ";
            type = types.str;
          };

          SIGNAL_STRENGTH_3 = mkOption {
            description = ''
              Set it to the icon you want to use for the signal strength 3.
            '';
            default = "󰤥 ";
            type = types.str;
          };

          SIGNAL_STRENGTH_4 = mkOption {
            description = ''
              Set it to the icon you want to use for the signal strength 4.
            '';
            default = "󰤨 ";
            type = types.str;
          };
        };
      };

      default = {};

      example = lib.literalExpression ''
        {
          LOCATION = 3;
          X_AXIS = 0;
          Y_AXIS = 0;
          NOTIFICATION = false;
          QRCODE_LOCATION = 3;
          QRCODE_DIR = "/tmp/";
          WIDTH_FIX_MAIN = 50;
          WIDTH_FIX_STATUS = 50;
          ASCII_OUT = false;
          CHANGE_BARS = false;
          SIGNAL_STRENGTH_0 = "󰤭 ";
          SIGNAL_STRENGTH_1 = "󰤟 ";
          SIGNAL_STRENGTH_2 = "󰤢 ";
          SIGNAL_STRENGTH_3 = "󰤥 ";
          SIGNAL_STRENGTH_4 = "󰤨 ";
        }
      '';
    };

    theme = mkOption {
      default = null;
      type = types.nullOr themeType;
      example = lib.literalExpression ''
        let
          # Use `mkLiteral` for string-like values that should show without
          # quotes, e.g.:
          # {
          #   foo = "abc"; => foo: "abc";
          #   bar = mkLiteral "abc"; => bar: abc;
          # };
          inherit (config.lib.formats.rasi) mkLiteral;
        in {
          "*" = {
            background-color = mkLiteral "#000000";
            foreground-color = mkLiteral "rgba ( 250, 251, 252, 100 % )";
            border-color = mkLiteral "#FFFFFF";
            width = 512;
          };

          "#inputbar" = {
            children = map mkLiteral [ "prompt" "entry" ];
          };

          "#textbox-prompt-colon" = {
            expand = false;
            str = ":";
            margin = mkLiteral "0px 0.3em 0em 0em";
            text-color = mkLiteral "@foreground-color";
          };
        }
      '';
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      lib.formats.rasi.mkLiteral = value: {
        _type = "literal";
        inherit value;
      };

      lib.formats.rasi.mkRef = value: {
        _type = "literal";
        value = "@${value}";
      };

      lib.formats.rasi.mkSimpleEl = bg: fg: {
        background-color = bg;
        text-color = fg;
      };

      home.packages = with pkgs; [
        rofi-network-manager
      ];

      xdg.configFile."rofi/rofi-network-manager.conf".text = toKeyValue cfg.settings;

      xdg.desktopEntries.rofi-network-manager = {
        name = "Rofi Network Manager";
        genericName = "Network Manager";
        exec = "rofi-network-manager";
        terminal = false;
        categories = ["Network"];
        icon = "network-wireless";
      };
    }
    (lib.mkIf (cfg.theme != false) {
      xdg.configFile."rofi/rofi-network-manager.rasi" =
        if (cfg.theme == null || !lib.isAttrs cfg.theme)
        then {
          source = "${pkgs.rofi-network-manager}/rofi-network-manager.rasi";
        }
        else {
          text = toRasi cfg.theme;
        };
    })
  ]);
}
