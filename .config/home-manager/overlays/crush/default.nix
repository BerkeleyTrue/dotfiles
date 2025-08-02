final: prev: {
  crush = prev.buildGoModule rec {
    pname = "crush";
    version = "0.2.0";

    src = prev.fetchFromGitHub {
      owner = "charmbracelet";
      repo = "crush";
      rev = "v${version}";
      hash = "sha256-EXKTc5kJa+4WnJtngUPXHE5oLp0hpYlyq81PuMEdKcY=";
    };

    vendorHash = "sha256-aI3MSaQYUOLJxBxwCoVg13HpxK46q6ZITrw1osx5tiE=";

    # Tests require config files that aren't available in the build environment
    doCheck = false;

    ldflags = [
      "-s"
      "-w"
      "-X=github.com/charmbracelet/crush/internal/version.Version=${version}"
    ];

    meta = {
      description = "The glamourous AI coding agent for your favourite terminal";
      homepage = "https://github.com/charmbracelet/crush";
      license = prev.lib.licenses.mit;
      maintainers = with prev.lib.maintainers; [ zimbatm ];
      mainProgram = "crush";
    };
  };
}
