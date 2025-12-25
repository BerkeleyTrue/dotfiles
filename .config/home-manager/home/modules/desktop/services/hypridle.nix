{
  pkgs,
  lib,
  ...
}: let
  getExe = lib.getExe;
  makoctl = getExe pkgs.mako;
  playerctl = getExe pkgs.playerctl;
  hyprlock = getExe pkgs.hyprlock;
  niri = getExe pkgs.niri;
  thirty_min = 1800;
in {
  services.hypridle = {
    enable = true;
    general = {
      lock_cmd = "pidof hyprlock || ${hyprlock}";
      on_lock_cmd = "${playerctl} pause; ${makoctl} set-mode do-not-disturb";
      on_unlock_cmd = "${makoctl} set-mode default; makoify -a 'Hephaestus' -u low -i distributor-logo-nixos 'Welcome Back!'";
      before_sleep_cmd = "loginctl lock-session";
      after_sleep_cmd = "${niri} msg action power-on-monitors";
    };
    listener = [
      {
        timeout = thirty_min; #
        on-timeout = "brightnessctl set 10";
        on-resume = "brightnessctl -r";
      }
      {
        timeout = thirty_min + 300; # 35min
        on-timeout = "loginctl lock-session";
      }
      {
        timeout = thirty_min + 330; # 5.5min
        on-timeout = "niri msg action power-off-monitors";
        on-resume = "niri msg action power-on-monitors && brightnessctl -r";
      }
      {
        timeout = thirty_min + 1800; # 60min
        on-timeout = "systemctl suspend";
      }
    ];
  };
}
