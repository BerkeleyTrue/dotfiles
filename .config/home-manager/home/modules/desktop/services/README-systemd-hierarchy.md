# X11 Systemd Service Hierarchy

This directory contains a hierarchical systemd service structure for managing X11/XMonad sessions.

## Service Hierarchy

```
x11-session.target (Root)
├── x11-environment.service (Environment setup)
├── x11-foundation.target (Foundation services)
│   ├── xresources.service (Load Xresources)
│   ├── xmodmap.service (Apply xmodmap)
│   ├── xkbmap.service (Keyboard layout)
│   └── xbindkeys.service (Hotkeys)
├── xmonad-session.target (Window manager session)
│   ├── xmonad.target (Window manager)
│   │   └── xmonad.service (XMonad WM)
│   └── desktop-services.target (Desktop services)
│       ├── compositor.target
│       │   └── picom.service
│       ├── notification.target
│       │   └── dunst.service
│       ├── wallpaper.target
│       │   └── nitrogen.service
│       ├── tray.target
│       │   ├── taffybar.service
│       │   ├── status-notifier-watcher.service
│       │   ├── flameshot.service
│       │   ├── pasystray.service
│       │   ├── tailscale-systray.service
│       │   └── blueman-applet.service
│       ├── desktop-utilities.target
│       │   └── unclutter.service
│       ├── input-services.target
│       │   ├── xcape.service
│       │   └── xplugd.service
│       └── xdg-desktop-portal-kde.service
├── watch-sleep.service (Sleep monitoring)
└── watch-lid.service (Lid monitoring)
```

## Usage

### Starting X11 Session
```bash
startx-systemd
```

### Manual Service Management
```bash
# Start the entire X11 session
systemctl --user start x11-session.target

# Start just XMonad
systemctl --user start xmonad.target
```

### Debugging
```bash
# View logs
journalctl --user -u x11-session.target
journalctl --user -u xmonad.service

# Check dependencies
systemctl --user list-dependencies x11-session.target
systemctl --user list-dependencies xmonad.target
```
