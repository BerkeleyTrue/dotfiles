{
  pkgs,
  lib,
  profile,
  ...
}: {
  home.packages = with pkgs; [
    kanata # intercept and transform keyboard inputs
  ];

  systemd.user.services.kanata = lib.mkIf (profile == "rena") {
    Unit = {
      Description = "Run kanata";
    };

    # requires udev rules for uinput
    # uinput should have 0660 permissions
    # be a part of the uinput group
    # and the current user be a part of the uinput group
    # you may also need /etc/modules-load.d/uinput.conf to have "uinput"
    # on arch systems
    Service = {
      Restart = "always";
      RestartSec = "3";
      ExecStart = "${pkgs.kanata}/bin/kanata";
      Nice = "-20";
    };

    Install = {
      WantedBy = ["default.target"];
    };
  };
}
