AUDIO_ENABLE = no

RGBLIGHT_ENABLE = no  # Clashes with audio
BLUETOOTH_ENABLE = no # No bluetooth
SLEEP_LED_ENABLE = no # Uses BACKLIGHT_ENABLE rimer
MOUSEKEY_ENABLE = yes # Use keyboard as mouse
MIDI_ENABLE = no

ifndef QUANTUM_DIR
	include ../../../../Makefile
endif
