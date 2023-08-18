{ stdenv, fetchgit }:

stdenv.mkDerivation {

  name = "rofi-network-manager";

  src = fetchgit {
    url = "https://github.com/P3rf/rofi-network-manager.git";
    rev = "19a3780fa3ed072482ac64a4e73167d94b70446b";
    sha256 = "03bizzfp2ibzaz9yc38svknk4ikz14pczv4qzmj0yzmh5vx2mbmh";
  };

  installPhase = ''
    mkdir --parents $out/bin
    cp ./rofi-network-manager.sh $out/bin
  '';

  meta = with stdenv.lib; {
    description = "A Network manager for Tiling Window Managers [i3/bspwm/awesome/etc] or not.";
    homepage = "https://github.com/P3rf/rofi-network-manager";
    maintainers = [];
    license = licenses.mit;
    platforms = platforms.linux;
  };

}
