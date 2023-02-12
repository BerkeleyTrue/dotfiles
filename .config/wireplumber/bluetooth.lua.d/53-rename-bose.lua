table.insert(bluez_monitor.rules, {
    matches = { { { "node.name", "equals", "bluez_output.4C_87_5D_29_57_5B.1" } } },
    apply_properties = {
        ["node.nick"] = "Bose",
        ["node.description"] = "Bose",
        ["device.alias"] = "Bose",
        ["bluez_monitor.enabled"] = false
    }
})
