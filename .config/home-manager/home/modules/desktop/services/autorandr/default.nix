{
  profile,
  hardware,
  ...
}: {
  programs = {
    autorandr = let
      profiles =
        if profile == "delora"
        then import ./delora.nix {inherit hardware;}
        else import ./rena.nix {inherit hardware;};
    in {
      enable = true;
      hooks = {
        postswitch = {
          notify = ''
            dunstify -a 'hephaestus' -u low -i distributor-logo-nixos 'Display profile' "$AUTORANDR_CURRENT_PROFILE"
          '';
          restart-taffy = ''
            systemctl --user restart taffybar
          '';
          restore-wallpaper = ''
            nitrogen --restore
          '';
        };
      };
      profiles = profiles;
    };
  };

  services.autorandr = {
    enable = true;
  };
}
