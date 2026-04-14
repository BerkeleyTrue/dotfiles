{inputs, ...}: {
  flake.modules.homeManager.pam-shim = {
    imports = [
      inputs.pam-shim.homeModules.default
    ];

    # Enable PAM shim to support auth in non-nixos environments
    pamShim.enable = true;
  };
}
