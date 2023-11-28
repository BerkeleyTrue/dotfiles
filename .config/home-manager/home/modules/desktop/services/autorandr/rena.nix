{hardware, ...}: let
  framework = "00ffffffffffff0009e5ca0b000000002f200104a51c137803de50a3544c99260f505400000001010101010101010101010101010101115cd01881e02d50302036001dbe1000001aa749d01881e02d50302036001dbe1000001a000000fe00424f452043510a202020202020000000fe004e4531333546424d2d4e34310a0073";
  screen = hardware.monitors.framework;
in {
  main = {
    fingerprint = {
      eDP-1 = framework;
    };

    config = {
      eDP-1 = {
        primary = true;
        crtc = 0;
        mode = "${builtins.toString screen.width}x${builtins.toString screen.height}";
        position = "0x0";
        rate = "60.00";
      };
    };
  };
}
