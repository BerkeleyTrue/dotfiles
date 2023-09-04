{
  formatting.command = "nixpkgs-fmt";
  options = {
    enable = true;
    target = {
      args = [];
      # home-manager configuration
      installable = "/flakeref#homeConfigurations.berkeleytrue.options";
    };
  };
}
