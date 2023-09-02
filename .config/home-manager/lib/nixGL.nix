{
  pkgs,
  lib,
}: pkg:
pkgs.runCommand "${pkg.name}-nixgl-wrapper" {} ''
  mkdir $out
  ln -s ${pkg}/* $out
  rm $out/bin
  mkdir $out/bin
  for bin in ${pkg}/bin/*; do
    wrapped_bin=$out/bin/$(basename $bin)
    echo "#!/bin/bash" > $wrapped_bin
    echo "exec ${lib.getExe pkgs.nixgl.nixGLIntel} $bin \$@" >> $wrapped_bin
    chmod +x $wrapped_bin
  done
''
