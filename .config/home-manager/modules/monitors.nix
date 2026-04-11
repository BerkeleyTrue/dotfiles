{...}: {
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
}
