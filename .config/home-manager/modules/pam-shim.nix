{inputs, ...}: {
  flake.modules.homeManager.pam-shim = {
    imports = [
      inputs.pam-shim.homeModules.default
    ];
  };
}
