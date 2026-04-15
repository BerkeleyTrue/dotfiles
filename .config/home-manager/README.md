# Niri/Wayland Systemd Service Hierarchy

This directory contains a hierarchical systemd service structure for managing Niri/Wayland sessions.

## Service Hierarchy

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

## Usage

### Starting Niri Session
```bash
startniri
```

### Manual Service Management
```bash
# Start Niri session
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
