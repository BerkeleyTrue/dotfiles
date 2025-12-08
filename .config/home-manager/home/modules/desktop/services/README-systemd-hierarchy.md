# Niri/Wayland Systemd Service Hierarchy

This directory contains a hierarchical systemd service structure for managing Niri/Wayland sessions.

## Service Hierarchy

```
graphical-session.target (systemd standard)
└── wayland-session.target (Root)
    ├── wayland-environment.service (Environment setup)
    ├── wayland-foundation.target (Foundation services)
    └── niri.target (Compositor)
        └── niri.service (Niri compositor)
        └── niri-session.target (Session)
            └── desktop-services.target (Desktop services)
                ├── notification.target
                │   └── notification-daemon.service (mako)
                ├── tray.target
                │   ├── waybar.service
                │   ├── status-notifier-watcher.service
                │   ├── flameshot.service
                │   ├── pasystray.service
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
# Start the entire Wayland session
systemctl --user start wayland-session.target

# Start just Niri
systemctl --user start niri.target
```

### Debugging
```bash
# View logs
journalctl --user -u wayland-session.target
journalctl --user -u niri.service

# Check dependencies
systemctl --user list-dependencies wayland-session.target
systemctl --user list-dependencies niri.target
```
