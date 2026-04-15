# Home Manager Flake

## Architecture

### flake-parts + Dendritic Modules

The flake is built on [flake-parts](https://github.com/hercules-ci/flake-parts), which structures outputs as a NixOS-style module system at the flake level. All files under `modules/` are automatically imported via [import-tree](https://github.com/vic/import-tree):

```nix
outputs = inputs @ {flake-parts, ...}:
  flake-parts.lib.mkFlake {inherit inputs;} {
    imports = [ (inputs.import-tree ./modules) ];
  };
```

Each file in `modules/` is a flake-parts module — it receives `self`, `inputs`, `config`, `lib`, etc. and contributes to shared flake-level options. This is the **dendritic pattern**: instead of one monolithic config, each concern (niri, waybar, kanata, monitors, …) is an independent module that branches off `flake.*` options simultaneously.

### Key options

| Option | Purpose |
|--------|---------|
| `flake.modules.homeManager.<name>` | Declares a reusable home-manager module |
| `configurations.home.<hostname>` | Wires modules together into a host configuration |
| `flake.homeConfigurations` | Final output consumed by `home-manager switch` |

A typical module looks like:

```nix
# modules/waybar.nix
{self, ...}: {
  flake.modules.homeManager.waybar = {config, pkgs, lib, ...}: {
    programs.waybar.enable = true;
    # ...
  };
}
```

Host configs (e.g. `modules/delora.nix`) then compose modules by name:

```nix
configurations.home.delora = {
  username = "berkeleytrue";
  system = "x86_64-linux";
  modules = with homeManager; [ niri waybar kanata fonts … ];
};
```

---

## Systemd / Wayland Session Setup

### Service Hierarchy

```
graphical-session.target (systemd standard)
├── wayland-environment.service (Environment setup)
├── niri.target (Compositor)
│   └── niri.service (Niri compositor)
└── desktop-services.target (Desktop services)
    ├── notification.target
    │   └── swaync.service
    ├── tray.target
    │   ├── waybar.service
    │   ├── status-notifier-watcher.service
    │   ├── flameshot.service
    │   ├── tailscale-systray.service
    │   └── blueman-applet.service
    └── xdg-desktop-portal-kde.service
```

### Starting the Session

```bash
startniri
# or
systemctl --user start niri.target
```

### Debugging

```bash
# View logs
journalctl --user -u niri.service
journalctl --user -u graphical-session.target

# Check dependencies
systemctl --user list-dependencies niri.target
systemctl --user list-dependencies graphical-session.target
```
