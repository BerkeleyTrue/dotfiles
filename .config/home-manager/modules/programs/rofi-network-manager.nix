{ pkgs, config, ... }:
{

  home.packages = with pkgs; [
    rofi-network-manager
  ];

  home.file."${config.xdg.configHome}/rofi/rofi-network-manager.rasi".source = "${pkgs.rofi-network-manager}/rofi-network-manager.rasi";
  home.file."${config.xdg.configHome}/rofi/rofi-network-manager.conf".source = "${pkgs.rofi-network-manager}/rofi-network-manager.conf";
}
