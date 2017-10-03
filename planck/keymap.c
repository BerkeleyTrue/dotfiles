/* Copyright 2015-2017 Jack Humbert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "planck.h"
#include "action_layer.h"
#include "version.h"

extern keymap_config_t keymap_config;

enum planck_layers {
  _QWERTY,
  _LOWER,
  _RAISE,
  _NAV,
  _KEYBOARD
};

enum planck_keycodes {
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
#define HYPRBRC     ALL_T(KC_RBRC)
#define HYPLBRC     ALL_T(KC_LBRC)
#define NAV_SPC     LT(_NAV, KC_SPC)
#define GUIGRAV     LGUI(KC_GRV)
#define DBLQUOT     S(KC_QUOT)

// Macros
enum planck_macros {
  M_VERSION,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

  /* Qwerty
  *                ┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
  *                │  ⇥  │  Q  │  W  │  E  │  R  │  T  │  Y  │  U  │  I  │  O  │  P  │ BSPC|
  *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
  * Tap for Esc -- │  ⌃  │  A  │  S  │  D  │  F  │  G  │  H  │  J  │  K  │  L  │  ;  │  '  │
  *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
  *   Tap for ( -- │  ⇧  │  Z  │  X  │  C  │  V  │  B  │  N  │  M  │  ,  │  .  │  /  │  ⇧  │ -- Tap for )
  *                ├─────┼─────┼─────┼─────┼─────┼─────┴─────┼─────┼─────┼─────┼─────┼─────┤
  *                │  [  │Hyper│  ⌥  │  ⌘  │  ↓  │   Space   │  ↑  │  ⌘  │  ⌥  │Hyper│  ]  │
  *                └─────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────┴─────┘
  *                        /     /                     /                   /     /
  *   Tap for ] [ --------'-----/---------------------/-------------------/-----'
  *   Tap for { } -------------'---------------------/-------------------'
  *   Hold for NAV ---------------------------------'
  */
  [_QWERTY] = {
    {  KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,   KC_T,    KC_Y,     KC_U,    KC_I,    KC_O,    KC_P,    KC_BSPC },
    {  CTL_ESC, KC_A,    KC_S,    KC_D,    KC_F,   KC_G,    KC_H,     KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT },
    {  KC_LSPO, KC_Z,    KC_X,    KC_C,    KC_V,   KC_B,    KC_N,     KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RSPC },
    {  KC_LBRC, HYPRBRC, KC_LALT, KC_LGUI, LOWER,  NAV_SPC, NAV_SPC,  RAISE,   KC_RGUI, KC_RALT, HYPLBRC, KC_RBRC }
  },

  /* Numeric layer
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
   * Application -- │ ⌘-` │ F1  │ F2  │ F3  │ F4  │ F5  │ F6  │ F7  │ F8  │ F9  │ F10 │  #  │
   *      window    ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
   *    switcher    │     │  1  │  2  │  3  │  4  │  5  │  6  │  7  │  8  │  9  │  0  │ RTN │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │  -  │  =  │  `  │  \  │  :  │     │     │  ,  │  .  │  /  │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┴─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │ Backspace │     │     │     │     │     │
   *                └─────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────┴─────┘
   */
  [_LOWER] = {
    {GUIGRAV, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,  KC_F10,   S(KC_3) },
    {_______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,   KC_0,     KC_ENT  },
    {_______, KC_MINS, KC_EQL,  KC_GRV,  KC_BSLS, KC_COLN, _______, _______, KC_COMM, KC_DOT, KC_SLSH,  _______ },
    {_______, _______, _______, _______, _______, KC_BSPC, KC_BSPC, _______, _______, _______, _______, _______ }
  },

  /* Symbol layer
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
   *                │     │ F11 │ F12 │ F13 │ F14 │ F15 │ F16 │ F17 │ F18 │ F19 │ F20 │  #  │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │  !  │  @  │  #  │  $  │  %  │  ^  │  &  │  *  │  '  │  "  │     │ \
   *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤  |-- Mostly shifted version
   *                │     │  _  │  +  │  ~  │  |  │  :  │     │     │  ,  │  .  │  /  │     │ /    of lower layer
   *                ├─────┼─────┼─────┼─────┼─────┼─────┴─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │  Delete   │     │     │     │     │     │
   *                └─────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────┴─────┘
   */
  [_RAISE] = {
    {_______, KC_F11,  KC_F12,  KC_F13,  KC_F14,  KC_F15,  KC_F16,  KC_F17,  KC_F18,  KC_F19,  KC_F20,  S(KC_3) },
    {_______, S(KC_1), S(KC_2), S(KC_3), S(KC_4), S(KC_5), S(KC_6), S(KC_7), S(KC_8), KC_QUOT, DBLQUOT, KC_ENT  },
    {_______, KC_UNDS, KC_PLUS, KC_TILD, KC_PIPE, KC_COLN, _______, _______, KC_COMM, KC_DOT,  KC_SLSH, _______ },
    {_______, _______, _______, _______, _______, KC_DEL,  KC_DEL,  _______, _______, _______, _______, _______ }
  },

  /* Nav
   *
   *         Mouse MVMT   /````````````````\             /```````````````````\----- Vim-style arrow keys
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
   *                │     │     │MS UP│     │     │     │     │     │     │     │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │MS LF│MS DN│MS RT│MSBT1│     │  ←  │  ↓  │  ↑  │  →  │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │MSBT2│     │     │MSWDN│MSWUP│     │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┴─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │           │     │     │     │     │     │
   *                └─────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────┴─────┘
   */
  [_NAV] = {
    {_______, ___x___, KC_MS_U, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, _______ },
    {_______, KC_MS_L, KC_MS_D, KC_MS_R, KC_BTN1, ___x___, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, ___x___, _______ },
    {_______, ___x___, ___x___, ___x___, KC_BTN2, ___x___, ___x___, MS_W_DN, MS_W_UP, ___x___, ___x___, _______ },
    {_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______ }
  },

  /* Keyboard settings layer
   *                ┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
   *    Firmware -- │     │Reset│     │     │     │     │     │     │     │     │Vers │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │     │     │     │     │     │     │     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
   *       Audio -- │     │Voic-│Voic+│Mus +│Mus -│MIDI+│MIDI-│     │     │Aud +│Aud -│     │
   *                ├─────┼─────┼─────┼─────┼─────┼─────┴─────┼─────┼─────┼─────┼─────┼─────┤
   *                │     │     │     │     │     │  Toggle   │     │Toggl│ BL- │ BL+ │     │
   *                └─────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────┴─────┘
   *                                                    \_____________\_ Backlight _/
   */
  [_KEYBOARD] = {
    {___x___, RESET,   ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
    {___x___, QWERTY,  ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
    {___x___, MUV_DE,  MUV_IN,  MU_ON,   MU_OFF,  MI_ON,   MI_OFF,  ___x___, ___x___, AU_ON,   AU_OFF,  ___x___},
    {___x___, ___x___, ___x___, ___x___, LOWER,   BL_TOGG, BL_TOGG, RAISE,   BL_TOGG, BL_DEC,  BL_INC,  ___x___}
  }

};


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
  }
  return true;
}
