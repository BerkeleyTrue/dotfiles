table.insert(alsa_monitor.rules, {
    matches = { { { "device.name", "equals", "alsa_card.pci-0000_09_00.1" } } },
    apply_properties = { ["device.disabled"] = true }
})
