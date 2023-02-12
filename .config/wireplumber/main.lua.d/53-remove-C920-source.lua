table.insert(alsa_monitor.rules, {
    matches = { { { "node.name", "equals", "alsa_input.usb-046d_HD_Pro_Webcam_C920_A77A0BEF-02.analog-stereo" } } },
    apply_properties = { ["node.disabled"] = true }
})
