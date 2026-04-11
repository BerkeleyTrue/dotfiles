{...}: {
  perSystem = {pkgs, ...}: {
    devShells.default = pkgs.mkShell {
      name = "home-manager";
      buildInputs = with pkgs; [
        just
      ];
      shellHook = ''
        function menu () {
          echo
          echo -e "\033[1;34m>==> ️  '$name'\n\033[0m"
          ${pkgs.just}/bin/just --list
          echo
          echo "(Run 'just --list' to display this menu again)"
          echo
        }

        menu
      '';
    };
  };
}
