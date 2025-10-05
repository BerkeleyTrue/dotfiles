{pkgs, ...}: {
  home.packages = with pkgs; [
    nitrogen
  ];

  systemd.user.services.nitrogen = {
    Unit = {
      Description = "Nitrogen wallpaper manager";
      Documentation = ["man:nitrogen(1)"];
      PartOf = ["wallpaper.target"];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStartPre = "${pkgs.coreutils}/bin/sleep 1";
      ExecStart = "${pkgs.nitrogen}/bin/nitrogen --restore";
      TimeoutStartSec = "10s";
    };

    Install = {
      WantedBy = ["wallpaper.target"];
    };
  };
}
