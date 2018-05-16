const foregroundColor = '#f8f8f2';
const backgroundColor = '#282a36';
const black = '#44475a';
const red = '#ff5555';
const green = '#50fa7b';
const yellow = '#f1fa8c';
const blue = '#6272a4';
const magenta = '#ff79c6';
const cyan = '#8be9fd';
const gray = '#666666';
const brightBlack = '#999999';
const brightWhite = '#ffffff';
module.exports = {
  config: {
    // default font size in pixels for all tabs
    fontSize: 12,

    // font family with optional fallbacks
    fontFamily: [
      '"DejaVuSansMono Nerd Font"',
      '"DroidSansMono Nerd Font"',
      '"Lucida Console"',
      'monospace',
    ].join(', '),

    // terminal cursor background color and opacity
    // (hex, rgb, hsl, hsv, hwb or cmyk)
    cursorColor: 'rgba(248,28,229,0.8)',

    // `BEAM` for |, `UNDERLINE` for _, `BLOCK` for █
    cursorShape: 'BLOCK',
    cursorBlink: true,

    // color of the text
    foregroundColor,

    // terminal background color
    backgroundColor,

    // border color (window, tabs)
    borderColor: '#333',

    // custom css to embed in the main window
    css: `
      .tabs_list .tab_tab.tab_active .tab_text  {
        background: ${backgroundColor};
      }
      .tab_active:before {
        border-color: rgb(68, 71, 90);
      }
    `,
    // custom padding (css format, i.e.: `top right bottom left`)
    padding: '1px',

    // the full list. If you're going to provide the full color palette,
    // including the 6 x 6 color cubes and the grayscale map, just provide
    // an array here instead of a color map object
    colors: {
      black,
      red,
      green,
      yellow,
      blue,
      magenta,
      cyan,
      gray,

      // bright
      brightBlack,
      brightWhite
    },

    // the shell to run when spawning a new session (i.e. /usr/local/bin/fish)
    // if left empty, your system's login shell will be used by default
    shell: 'zsh',

    // for setting shell arguments
    // (i.e. for using interactive shellArgs: ['-i'])
    // by default ['--login'] will be used
    shellArgs: ['--login'],

    // for environment variables
    env: {},

    // set to false for no bell
    bell: 'SOUND',

    // if true, selected text will automatically be copied to the clipboard
    copyOnSelect: false,

    // URL to custom bell
    // bellSoundURL: 'http://example.com/bell.mp3',

    // for advanced config flags please refer to https://hyper.is/#cfg
    hyperBorder: {
      animate: true,
      borderWidth: '2px',
      borderColors: [magenta, cyan],
      blurredColors: ['#000000']
    }
  },
  // end config

  // order here is important
  plugins: [
    'hyperborder',
    'hyperfull',
    'hyperterm-cursor',
    'hyper-disable-new-version-notification',
    'hyperminimal'
  ],

  // in development, you can create a directory under
  // `~/.hyper_plugins/local/` and include it here
  // to load it and avoid it being `npm install`ed
  localPlugins: [],
  modifierKeys: {
    altIsMeta: true
  },
};
