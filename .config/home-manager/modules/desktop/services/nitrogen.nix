{pkgs, ...}: {
  home.packages = with pkgs; [
    nitrogen
  ];

  systemd.user.services.nitrogen = {
    Unit = {
      Description = "Nitrogen wallpaper manager";
      After = ["picom.service"];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      Type = "oneshot";
      ExecStartPre = "sleep 1s";
      ExecStart = "${pkgs.nitrogen}/bin/nitrogen --restore";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
