{
  profile,
  lib,
  common-modules,
  hardware,
  ...
}:
if profile == "delora"
then
  import ./delora.nix
  {
    inherit lib common-modules;
    inherit (hardware) monitors;
  }
else
  import ./rena.nix
  {
    inherit lib common-modules;
    inherit (hardware) monitors;
  }
