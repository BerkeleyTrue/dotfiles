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

    # requires udev rules for uinput group
    # getent group uinput (to check if the group exists and is system group, gid < 1000)
    # sudo groupadd -r uinput (to create system group if it doesn't exist)
    # /dev/uinput should have 0660 permissions
    # 'KERNEL=="uinput", MODE:="0660", GROUP:="uinput", OPTIONS+="static_node=uinput"' in /etc/udev/rules.d/99-uinput.rules
    # user must be a part of the uinput group (sudo usermod -aG uinput $USER)
    # sudo udevadm control --reload-rules && sudo udevadm trigger (to reload udev rules)
    # reboot or re-login to apply group changes
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
