{
  flake.monitor_utils = {
    mkMonitor = {
      height,
      width,
      label,
      rate,
      scale ? 1.0,
      position ? {
        x = 0;
        y = 0;
      },
    }: {
      inherit height width label rate scale position;
      logical = {
        height = builtins.floor (height / scale);
        width = builtins.floor (width / scale);
      };
    };

    centerSelfOnBase = base: self: {
      x = base.position.x + (base.logical.width - self.logical.width) / 2;
      y = 0;
    };
  };

  flake.modules.homeManager.monitor = {lib, ...}: {
    options.monitors = lib.mkOption {
      default = {};
      type = lib.types.attrsOf (lib.types.submodule ({config, ...}: {
        options = {
          height = lib.mkOption {type = lib.types.int;};
          width = lib.mkOption {type = lib.types.int;};
          label = lib.mkOption {type = lib.types.str;};
          rate = lib.mkOption {type = lib.types.number;};
          scale = lib.mkOption {
            type = lib.types.float;
            default = 1.0;
          };
          position = lib.mkOption {
            default = {
              x = 0;
              y = 0;
            };
            type = lib.types.submodule {
              options = {
                x = lib.mkOption {
                  type = lib.types.number;
                  default = 0;
                };
                y = lib.mkOption {
                  type = lib.types.number;
                  default = 0;
                };
              };
            };
          };
          logical = lib.mkOption {
            readOnly = true;
            type = lib.types.submodule {
              options = {
                height = lib.mkOption {type = lib.types.int;};
                width = lib.mkOption {type = lib.types.int;};
              };
            };
          };
        };
        config.logical = {
          height = builtins.floor (config.height / config.scale);
          width = builtins.floor (config.width / config.scale);
        };
      }));
    };
  };
}
