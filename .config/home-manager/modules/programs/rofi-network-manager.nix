{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.programs.rofi-network-manager;
  toKeyValue = generators.toKeyValue {
    listsAsDuplicateKeys = true;
  };
in
{
  options.programs.rofi-network-manager = {
    enable = mkEnableOption "rofi-network-manager: network manager in rofi";
    settings = mkOption {
      description = mdDoc ''
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
            default = 50;
            type = types.int;
          };

          WIDTH_FIX_STATUS = mkOption {
            description = ''
              WIDTH_FIX_MAIN/WIDTH_FIX_STATUS

              These values can be adjusted if the text doesn't fit or
              if there is too much space at the end when you launch the script.
              It will depend on the font type and size.
            '';
            default = 50;
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

      default = { };

      example = literalExpression ''
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
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      rofi-network-manager
    ];
    home.file."${config.xdg.configHome}/rofi/rofi-network-manager.rasi".source = "${pkgs.rofi-network-manager}/rofi-network-manager.rasi";
    home.file."${config.xdg.configHome}/rofi/rofi-network-manager.conf".text = toKeyValue cfg.settings;
  };
}
