/* Berkeleys Let's Split keymap */
#include "lets_split.h"
#include "action_layer.h"
#include "eeconfig.h"

extern keymap_config_t keymap_config;

enum _layers {
  _QWERTY,
  _CAP,
  _LOWER,
  _RAISE,
  _NAV,
  _KEYBOARD
};

enum _keycodes {
  QWERTY = SAFE_RANGE,
  LOWER,
  RAISE,
  NAV,
  KEYBOARD
};

// Key aliases for legibility
#define _______     KC_TRNS
#define ___x___     KC_NO

// Custom macros
#define CTL_ESC     CTL_T(KC_ESC)   // Tap for Esc, hold for Ctrl
#define MS_W_UP     KC_MS_WH_UP
#define MS_W_DN     KC_MS_WH_DOWN
#define NAV_SPC     LT(_NAV, KC_SPC)
#define KYBRBRC     LT(_KEYBOARD, KC_RBRC)
#define GUIGRAV     LGUI(KC_GRV)
#define DBLQUOT     S(KC_QUOT)
#define SFT_TAB     S(KC_TAB)


const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

  /* Qwerty
  *                ┌─────┬─────┬─────┬─────┬─────┬─────┐    ┌─────┬─────┬─────┬─────┬─────┬─────┐
  *                │  ⇥  │  q  │  w  │  e  │  r  │  t  │    │  y  │  u  │  i  │  o  │  p  │ ⇧ ⇥ |
  *                ├─────┼─────┼─────┼─────┼─────┼─────┤    ├─────┼─────┼─────┼─────┼─────┼─────┤
  * Tap for Esc -- │  ⌃  │  a  │  s  │  d  │  f  │  g  │    │  h  │  j  │  k  │  l  │  ;  │  '  │
  *                ├─────┼─────┼─────┼─────┼─────┼─────│    ├─────┼─────┼─────┼─────┼─────┼─────┤
  *   Tap for ( -- │  ⇧  │  z  │  x  │  c  │  v  │  b  │    │  n  │  m  │  ,  │  .  │  /  │  ⇧  │ -- Tap for )
  *                ├─────┼─────┼─────┼─────┼─────┼─────│    ├─────┼─────┼─────┼─────┼─────┼─────┤
  *                │  [  │  ]  │  ⌥  │  ⌘  │  ↓  │ SPC │    │ SPC │  ↑  │  ⌘  │  ⌥  │  [  │  ]  │
  *                └─────┴─────┴─────┴─────┴─────┴─────┘    └─────┴─────┴─────┴─────┴─────┴─────┘
  *                        /                       /          /                       /
  *   Hold for Keyboard---'-----------------------/----------/-----------------------'
  *   Hold for NAV ------------------------------'----------'
  */
  [_QWERTY] = {
    {  KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,   KC_T,    KC_Y,     KC_U,    KC_I,    KC_O,    KC_P,    SFT_TAB},
    {  CTL_ESC, KC_A,    KC_S,    KC_D,    KC_F,   KC_G,    KC_H,     KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT },
    {  KC_LSPO, KC_Z,    KC_X,    KC_C,    KC_V,   KC_B,    KC_N,     KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RSPC },
    {  KC_LBRC, KYBRBRC, KC_LALT, KC_LGUI, LOWER,  NAV_SPC, NAV_SPC,  RAISE,   KC_RGUI, KC_RALT, KYBRBRC, KC_RBRC }
  },

  /* CAP (shifted)
  *                ┌─────┬─────┬─────┬─────┬─────┬─────┐    ┌─────┬─────┬─────┬─────┬─────┬─────┐
  *                │     │  Q  │  W  │  E  │  R  │  T  |    │  Y  │  U  │  I  │  O  │  P  │     |
  *                ├─────┼─────┼─────┼─────┼─────┼─────┤    ├─────┼─────┼─────┼─────┼─────┼─────┤
  *                │     │  A  │  S  │  D  │  F  │  G  │    │  H  │  J  │  K  │  L  │  :  │  "  │
  *                ├─────┼─────┼─────┼─────┼─────┼─────┤    ├─────┼─────┼─────┼─────┼─────┼─────┤
  *                │  (  │  Z  │  X  │  C  │  V  │  B  │    │  N  │  M  │  <  │  >  │  ?  │  )  │
  *                ├─────┼─────┼─────┼─────┼─────┼─────┤    ├─────┼─────┼─────┼─────┼─────┼─────┤
  *                │     │     │     │     │     │ SPC │    │ SPC │     │     │     │     │     │
  *                └─────┴─────┴─────┴─────┴─────┴─────┘    └─────┴─────┴─────┴─────┴─────┴─────┘
  */
  [_CAP] = {
    {  _______, S(KC_Q), S(KC_W), S(KC_E), S(KC_R), S(KC_T), S(KC_Y), S(KC_U), S(KC_I),    S(KC_O),   S(KC_P),    _______    },
    {  _______, S(KC_A), S(KC_S), S(KC_D), S(KC_F), S(KC_G), S(KC_H), S(KC_J), S(KC_K),    S(KC_L),   S(KC_SCLN), S(KC_QUOT) },
    {  S(KC_9), S(KC_Z), S(KC_X), S(KC_C), S(KC_V), S(KC_B), S(KC_N), S(KC_M), S(KC_COMM), S(KC_DOT), S(KC_SLSH), S(KC_0)    },
    {  _______, _______, _______, _______, _______, KC_SPC, KC_SPC,   _______, _______,    _______,   _______,    _______    }
  },

  /* Numeric layer
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┐   ┌─────┬─────┬─────┬─────┬─────┬─────┐
   * Application -- │ ⌘-` │ F1  │ F2  │ F3  │ F4  │ F5  │   │ F6  │ F7  │ F8  │ F9  │ F10 │     │
   *      window    ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *    switcher    │     │  1  │  2  │  3  │  4  │  5  │   │  6  │  7  │  8  │  9  │  0  │ RTN │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │  -  │  =  │  `  │  \  │  :  │   │     │     │  ,  │  .  │  /  │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │ BSPC│   │ BSPC│     │     │     │     │     │
   *                └─────┴─────┴─────┴─────┴─────┴─────┘   └─────┴─────┴─────┴─────┴─────┴─────┘
   */
  [_LOWER] = {
    {GUIGRAV, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,  KC_F10,   _______ },
    {_______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,   KC_0,     KC_ENT  },
    {_______, KC_MINS, KC_EQL,  KC_GRV,  KC_BSLS, KC_COLN, _______, _______, KC_COMM, KC_DOT, KC_SLSH,  _______ },
    {_______, _______, _______, _______, _______, KC_BSPC, KC_BSPC, _______, _______, _______, _______, _______ }
  },

  /* Symbol layer
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┐   ┌─────┬─────┬─────┬─────┬─────┬─────┐
   *                │     │ F11 │ F12 │ F13 │ F14 │ F15 │   │ F16 │ F17 │ F18 │ F19 │ F20 │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │  !  │  @  │  #  │  $  │  %  │   │  ^  │  &  │  *  │  '  │  "  │     │ \
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤  |-- Mostly shifted version
   *                │     │  _  │  +  │  ~  │  |  │  :  │   │     │     │  ,  │  .  │  /  │     │ /    of lower layer
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │ Del │   │ Del │     │     │     │     │     │
   *                └─────┴─────┴─────┴─────┴─────┴─────┘   └─────┴─────┴─────┴─────┴─────┴─────┘
   */
  [_RAISE] = {
    {_______, KC_F11,  KC_F12,  KC_F13,  KC_F14,  KC_F15,  KC_F16,  KC_F17,  KC_F18,  KC_F19,  KC_F20,  _______ },
    {_______, S(KC_1), S(KC_2), S(KC_3), S(KC_4), S(KC_5), S(KC_6), S(KC_7), S(KC_8), KC_QUOT, DBLQUOT, KC_ENT  },
    {_______, KC_UNDS, KC_PLUS, KC_TILD, KC_PIPE, KC_COLN, _______, _______, KC_COMM, KC_DOT,  KC_SLSH, _______ },
    {_______, _______, _______, _______, _______, KC_DEL,  KC_DEL,  _______, _______, _______, _______, _______ }
  },

  /* Nav
   *
   *         Mouse MVMT   /````````````````\                  /```````````````````\----- Vim-style arrow keys
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┐   ┌─────┬─────┬─────┬─────┬─────┬─────┐
   *                │     │     │MS UP│     │     │     │   │     │     │     │     │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │MS LF│MS DN│MS RT│MSBT1│     │   │  ←  │  ↓  │  ↑  │  →  │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │MSBT2│     │   │     │MSWDN│MSWUP│     │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │     │   │     │     │     │     │     │     │
   *                └─────┴─────┴─────┴─────┴─────┴─────┘   └─────┴─────┴─────┴─────┴─────┴─────┘
   */
  [_NAV] = {
    {_______, ___x___, KC_MS_U, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, _______ },
    {_______, KC_MS_L, KC_MS_D, KC_MS_R, KC_BTN1, ___x___, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, ___x___, KC_ENT  },
    {_______, ___x___, ___x___, ___x___, KC_BTN2, ___x___, ___x___, MS_W_DN, MS_W_UP, ___x___, ___x___, _______ },
    {_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______ }
  },

  /* Keyboard settings layer
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┐   ┌─────┬─────┬─────┬─────┬─────┬─────┐
   *    Firmware -- │     │Reset│     │     │     │     │   │     │     │     │     │Vers │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │     │   │     │     │     │     │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *       Audio -- │     │Voic-│Voic+│Mus +│Mus -│MIDI+│   │MIDI-│     │     │Aud +│Aud -│     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┤   ├─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │Toggl│   │Toggl│     │Toggl│ BL- │ BL+ │     │
   *                └─────┴─────┴─────┴─────┴─────┴─────┘   └─────┴─────┴─────┴─────┴─────┴─────┘
   *                                                  \_________\__________\_ Backlight _/
   */
  [_KEYBOARD] = {
    {___x___, RESET,   ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
    {___x___, QWERTY,  ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
    {___x___, MUV_DE,  MUV_IN,  MU_ON,   MU_OFF,  MI_ON,   MI_OFF,  ___x___, ___x___, AU_ON,   AU_OFF,  ___x___},
    {___x___, ___x___, ___x___, ___x___, LOWER,   BL_TOGG, BL_TOGG, RAISE,   BL_TOGG, BL_DEC,  BL_INC,  ___x___}
  }
};

static bool RGUI_PRESSED = false;
static bool LGUI_PRESSED = false;
bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    case LOWER:
      if (record->event.pressed) {
        layer_on(_LOWER);
        update_tri_layer(_LOWER, _RAISE, _KEYBOARD);
      } else {
        layer_off(_LOWER);
        update_tri_layer(_LOWER, _RAISE, _KEYBOARD);
      }
      return false;
      break;
    case RAISE:
      if (record->event.pressed) {
        layer_on(_RAISE);
        update_tri_layer(_LOWER, _RAISE, _KEYBOARD);
      } else {
        layer_off(_RAISE);
        update_tri_layer(_LOWER, _RAISE, _KEYBOARD);
      }
      return false;
      break;
    case KYBRD:
      if (record->event.pressed) {
        layer_on(_KEYBOARD);
      } else {
        layer_off(_KEYBOARD);
      }
      return false;
      break;
    case KC_RGUI:
      // set to pressed
      RGUI_PRESSED = record->event.pressed;
      // if left gui pressed and right gui pressed
      // turn on cap layer
      if (LGUI_PRESSED && RGUI_PRESSED) {
        // remove left gui mod
        unregister_code(KC_LGUI);
        layer_on(_CAP);
        // don't send gui in this case
        return false;
      } else {
        // if not both gui keys are pressed
        // then ensure cap layer is off
        layer_off(_CAP);
        // send gui button
        return true;
      }
      break;
    case KC_LGUI:
      // same as above
      LGUI_PRESSED = record->event.pressed;
      if (LGUI_PRESSED && RGUI_PRESSED) {
        // remove right gui mod
        unregister_code(KC_RGUI);
        layer_on(_CAP);
        return false;
      } else {
        layer_off(_CAP);
        return true;
      }
      break;
  }
  return true;
}
