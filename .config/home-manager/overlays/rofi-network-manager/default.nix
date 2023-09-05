final: prev: let
  pkgs = prev.pkgs;
in {
  rofi-network-manager = pkgs.stdenv.mkDerivation {
    name = "rofi-network-manager";

    src = pkgs.fetchgit {
      url = "https://github.com/P3rf/rofi-network-manager.git";
      rev = "19a3780fa3ed072482ac64a4e73167d94b70446b";
      sha256 = "03bizzfp2ibzaz9yc38svknk4ikz14pczv4qzmj0yzmh5vx2mbmh";
    };

    installPhase = ''
      mkdir --parents $out/bin
      cp ./rofi-network-manager.sh $out/bin/rofi-network-manager
      cp ./rofi-network-manager.rasi $out/rofi-network-manager.rasi
      cp ./rofi-network-manager.conf $out/rofi-network-manager.conf
    '';
  };
}
