# KMONAD

__Deprecated__

Using kanata through home-manager now

[kanata.nix](../home-manager/home/modules/desktop/services/kanata.nix)

--

* This needs to be run as root in order to work.
* Make edits to config file
  * Test with "time 10s kmonad .kmonad.kbd"
  * If everything looks good, copy to /etc/kmonad
  * Copy systemd service to /etc/systemd/system/
  * Start with `sudo systemctl start kmonad.service`
  * Enable to have it auto start `sudo systemctl enable kmonad.service`
