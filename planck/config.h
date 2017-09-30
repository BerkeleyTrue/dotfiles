#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../qmk_firmware/keyboards/planck/config.h"

#define PREVENT_STUCK_MODIFIERS

#ifdef AUDIO_ENABLE
    #define STARTUP_SONG SONG(PLANCK_SOUND)
    // #define STARTUP_SONG SONG(NO_SOUND)

    #define DEFAULT_LAYER_SONGS { SONG(QWERTY_SOUND), \
                                  SONG(COLEMAK_SOUND), \
                                  SONG(DVORAK_SOUND) \
                                }
#endif

#endif
