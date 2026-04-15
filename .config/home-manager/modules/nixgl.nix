{inputs, ...}: {
  flake.modules.homeManager.nixgl = {
    targets.genericLinux.nixGL.packages = inputs.nixgl.packages;
  };
}
