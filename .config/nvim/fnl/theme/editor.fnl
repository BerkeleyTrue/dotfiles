(module theme.editor
  {require
   {a aniseed.core
    r r
    hl utils.highlights
    utils utils}
   require-macros [macros]})

(defn main [{: c : s : add-group}]
  (add-group :ColorColumn       c.none c.surface0) ; used for the columns set with 'colorcolumn'
  (add-group :Conceal           c.overlay1) ; placeholder characters substituted for concealed text (see 'conceallevel');
  (add-group :Cursor            c.base c.text) ; character under the cursor
  (add-group :lCursor           c.overlay1) ; the character under the cursor when |language-mapping| is used (see 'guicursor')
  (add-group :CursorIM          c.base c.text) ; like Cursor, but used when in IME mode |CursorIM|
  (add-group :CursorColumn      c.none c.mantle) ; Screen-column at the cursor, when 'cursorcolumn' is set.
  (add-group :CursorLine        c.none (c.surface0:dark)) ; Screen-line at the cursor, when 'cursorline' is set.

  (add-group :DiffAdd           (c.green:dark)) ; diff mode: Added line |diff.txt|
  (add-group :DiffChange        c.blue) ; diff mode: Changed text within a changed line |diff.txt|
  (add-group :DiffDelete        (c.red:dark)) ; diff mode: Deleted line |diff.txt|
  (add-group :DiffText          (c.blue:dark)) ; diff mode: Changed text within a changed line |diff.txt|

  (add-group :Directory         c.blue) ; directory names (and other special names in listings)
  (add-group :EndOfBuffer       c.text c.base) ; filler lines (~) after the end of the buffer.  By default, this is highlighted like |hl-NonText|.
  (add-group :ErrorMsg          c.red c.none s.bold) ; error messages on the command line
  (add-group :Folded            c.blue c.surface1) ; line used for closed folds
  (add-group :FoldColumn        c.overlay0); column used for 'foldcolumn'
  (add-group :SignColumn        c.surface1) ; column where signs are displayed
  (add-group :SignColumnSB      c.surface1 c.crust) ; column where signs are displayed
  (add-group :Substitute        c.pink c.surface1) ; |:substitute| replacement text highlighting
  (add-group :LineNR            c.surface1) ; Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
  (add-group :CursorLineNr      c.lavender) ; Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
  (add-group :MatchParen        c.peach c.surface1 s.bold) ; The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
  (add-group :ModeMsg           c.text c.none s.bold) ; 'showmode' message (e.g., "-- INSERT -- ")
  (add-group :MsgSeparator      c.none c.none) ; Separator for scrolled messages, `msgsep` flag of 'display'
  (add-group :MoreMsg           c.blue) ; |more-prompt|
  (add-group :NonText           c.overlay0) ; '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line).
  (add-group :Normal            c.text c.base) ; normal text
  (add-group :NormalNC          c.text c.base) ; normal text in non-current windows
  (add-group :NormalSB          c.text c.crust) ; normal text in non-current windows
  (add-group :NormalFloat       c.text c.mantle) ; normal text in floating windows
  (add-group :FloatBorder       c.blue)
  (add-group :FloatTitle        c.subtext0) ; float window title
  (add-group :Pmenu             c.overlay2 (c.surface0:dark)) ; Popup menu: normal item.
  (add-group :PmenuSel          c.none c.surface1 s.bold) ; Popup menu: selected item.
  (add-group :PmenuSbar         c.none c.surface1) ; Popup menu: scrollbar.
  (add-group :PmenuThumb        c.none c.overlay2) ; Popup menu: Thumb of the scrollbar.
  (add-group :Question          c.blue)
  (add-group :QuickFixLine      c.none c.surface1 s.bold) ; Current |quickfix| item in the quickfix window. Combined with |hl-CursorLine| when the cursor is there.
  (add-group :Search            c.text (c.sky:dark)) ; Last search pattern highlighting (see 'hl-IncSearch').  Also used for similar items that need to stand out.
  (add-group :IncSearch         c.mantle (c.sky:dark)) ; 'incsearch' highlighting; also used for the text replaced with ":s///c"
  (add-group :CurSearch         c.mantle c.red) ; 'cursearch' highlighting: highlights the current search you're on differently
  (hl.link!  :SpecialKey        :NonText) ; Unprintable characters: text displayed differently from what it really is.  But not 'listchars' whitespace. |hl-Whitespace|
  (add-group :SpellBad          c.none c.none s.undercurl) ; Word that is not recognized by the spellchecker. |spell| Combined with the highlighting used otherwise.
  (add-group :SpellCap          c.none c.none s.undercurl) ; Word that should start with a capital. |spell| Combined with the highlighting used otherwise.
  (add-group :SpellLocal        c.none c.none s.undercurl) ; Word that is recognized by the spellchecker as one that is used in another region. |spell| Combined with the highlighting used otherwise.
  (add-group :SpellRare         c.none c.none s.undercurl) ; Word that is recognized by the spellchecker as one that is hardly ever used.  |spell| Combined with the highlighting used otherwise.
  (add-group :Statusline        c.text c.mantle) ; status line of current window
  (add-group :StatusLineNC      c.surface1 c.mantle) ; status lines of not-current windows
  (hl.link!  :StatusLineTerm    :StatusLine) s.bold ; status line of current terminal window
  (hl.link!  :StatusLineTermNC  :StatusLineNC) ; status lines of not-current terminal windows
  (add-group :TabLine           c.surface1 c.mantle) ; tab pages line, not active tab page label
  (add-group :TabLineFill       c.none c.none) ; tab pages line, where there are no labels
  (add-group :TabLineSel        c.green c.surface1) ; tab pages line, active tab page label
  (add-group :Title             c.blue c.none s.bold) ; titles for output from ":set all", ":autocmd" etc.
  (add-group :VertSplit         c.none c.none) ; the column separating vertically split windows
  (add-group :Visual            c.none c.surface1 s.bold) ; Visual mode selection
  (add-group :VisualNOS         c.none c.surface1 s.bold) ; Visual mode selection when vim is "Not Owning the Selection".
  (add-group :WarningMsg        c.yellow) ; warning messages
  (add-group :Whitespace        c.surface1) ; "nbsp", "space", "tab" and "trail" in 'listchars'
  (add-group :WildMenu          c.none c.overlay0) ; current match in 'wildmenu' completion
  (add-group :WinBar            c.none c.bglighter s.bold)

  ; status line helpers for separators
  (add-group :BerksStatusLineMod          c.yellow c.mantle)
  (add-group :BerksStatusLineModInverse   c.mantle c.yellow)
  (add-group :BerksStatusLineInfo         c.blue c.mantle)
  (add-group :BerksStatusLineInfoInverse  c.mantle c.blue)
  (add-group :BerksStatusLineErr          c.red c.mantle)
  (add-group :BerksStatusLineErrInverse   c.mantle c.red))
