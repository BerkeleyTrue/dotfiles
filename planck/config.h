#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../qmk_firmware/keyboards/planck/config.h"

#define PREVENT_STUCK_MODIFIERS
#define MOUSEKEY_DELAY             200
#define MOUSEKEY_INTERVAL          15
#define MOUSEKEY_MAX_SPEED         9
#define MOUSEKEY_TIME_TO_MAX       70
#define MOUSEKEY_WHEEL_MAX_SPEED   8
#define MOUSEKEY_WHEEL_TIME_TO_MAX 40

#ifdef AUDIO_ENABLE
    #define STARTUP_SONG SONG(PLANCK_SOUND)
    // #define STARTUP_SONG SONG(NO_SOUND)

    #define DEFAULT_LAYER_SONGS { SONG(QWERTY_SOUND), \
                                  SONG(COLEMAK_SOUND), \
                                  SONG(DVORAK_SOUND) \
                                }
#endif

#endif
