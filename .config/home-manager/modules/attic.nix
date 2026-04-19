{
  flake.modules.homeManager.attic = {
    config,
    pkgs,
    ...
  }: let
    attic = pkgs.attic-client; # Multi-tenant Nix Binary Cache
    token = config.sops.secrets.attic-bt-token;
    server = "https://atticd.r3dm.com";
    loginScript = pkgs.writeShellApplication {
      name = "attic-login";
      runtimeInputs = [attic];
      text = ''
        attic login homelab ${server} "$(cat ${token.path})"
      '';
    };
  in {
    sops.secrets.attic-bt-token = {};

    home.packages = [loginScript];
  };
}
