{}:

let overlays = (self: original: {
  haskell-language-server = original.haskell-language-server.override {
    supportedGhcVersions = ["925"];
  };
});

in

{ inherit overlays; }
