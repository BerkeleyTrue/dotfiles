{
  pkgs,
  theme,
  ...
}: let
  c = theme.colors;
in {
  services.mako = {
    enable = false;
    settings = {
      layer = "overlay";
      anchor = "top-right";
      width = 350;
      margin = 15;
      padding = 12;
      border-size = 4;
      border-radius = 8;
      font = "FiraCode Nerd Font Mono 10";
      format = ''<b>%a</b>: <i>%s</i>\n%b'';
      default-timeout = 5000;
      max-history = 100;

      # note: mako doesn't currently use scaleable icons, so many are not available
      # see: https://github.com/emersion/mako/issues/383
      icon-path = "${pkgs.dracula-icon-theme}/share/icons/Dracula:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark";

      text-color = c.text;
      background-color = c.base;
      border-color = c.mauve;
      progress-color = c.blue;

      "urgency=low" = {
        background-color = c.surface1;
        text-color = c.subtext1;
      };

      "urgency=normal" = {
        background-color = c.base;
        text-color = c.text;
      };

      "urgency=high" = {
        background-color = c.red;
        text-color = c.surface1;
        border-color = c.rosewater;
      };

      "mode=dnd" = {
        invisible = 1;
      };
    };
  };
}
