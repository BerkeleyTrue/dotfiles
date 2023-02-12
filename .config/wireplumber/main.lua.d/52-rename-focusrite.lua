table.insert(alsa_monitor.rules, {
    matches = {
        {
            {
                "node.name", "equals",
                "alsa_output.usb-Focusrite_Scarlett_2i2_USB-00.analog-stereo"
            }
        }
    },
    apply_properties = {
        ["node.nick"] = "Focusrite",
        ["node.description"] = "Focusrite"
    }
})

table.insert(alsa_monitor.rules, {
    matches = {
        {
            {
                "node.name", "equals",
                "alsa_input.usb-Focusrite_Scarlett_2i2_USB-00.analog-stereo"
            }
        }
    },
    apply_properties = {
        ["node.nick"] = "Focusrite",
        ["node.description"] = "Focusrite"
    }
})
