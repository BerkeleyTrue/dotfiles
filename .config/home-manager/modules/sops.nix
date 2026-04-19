{inputs, ...}: {
  flake.modules.homeManager.sops = {config, ...}: {
    imports = [
      inputs.sops-nix.homeManagerModules.sops
    ];
    sops = {
      defaultSopsFile = ../secrets/secrets.yml;
      age = {
        sshKeyPaths = [
          "/home/${config.home.username}/.ssh/id_ed25519"
        ];
        keyFile = "/home/${config.home.username}/.config/sops/age/keys.txt";
      };
    };
  };
}
