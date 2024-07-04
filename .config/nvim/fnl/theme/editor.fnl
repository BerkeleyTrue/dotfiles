(module theme.editor
  {autoload
   {a aniseed.core
    r r
    cb colorbuddy
    p theme.palette
    cl lib.color
    hl utils.highlights
    utils utils}
   require {}
   require-macros [macros]})

(comment
  (def c cb.colors))

(set-hl :Normal    {:fg (cl.->hex p.text) :bg (cl.->hex p.base)})
(set-hl :Search    {:fg (cl.->hex p.text) :bg (cl.->hex [189 48 30])}) ; Last search pattern highlighting (see 'hl-Search').  Also used for similar items that need to stand out.
(set-hl :CurSearch {:fg (cl.->hex p.mantle) :bg (cl.->hex [359 68 79])}) ; 'cursearch' highlighting: highlights the current search you're on differently
(set-hl :IncSearch {:fg (cl.->hex p.mantle) :bg (cl.->hex [189 48 35])}) ; Last search pattern highlighting (see 'hl-IncSearch').  Also used for similar items that need to stand out.
(set-hl :WildMenu  {:fg :fg :bg (cl.->hex p.overlay0)}) ; current match in 'wildmenu' completion
(set-hl :Folded    {:fg (cl.->hex p.blue)}) ; line used for closed folds

(set-hl :ColorColumn       {:fg p.none :bg (cl.->hex p.surface0)}) ; used for the columns set with 'colorcolumn'
(set-hl :Conceal           {:fg (cl.->hex p.overlay1)}) ; placeholder characters substituted for concealed text (see 'conceallevel');
(set-hl :Cursor            {:fg :fg :bg (cl.->hex p.text)}) ; character under the cursor
(set-hl :lCursor           {:fg (cl.->hex p.overlay1)}) ; the character under the cursor when |language-mapping| is used (see 'guicursor')
(set-hl :CursorIM          {:fg :fg :bg (cl.->hex p.text)}) ; like Cursor, but used when in IME mode |CursorIM|

(set-hl :CursorColumn      {:fg p.none :bg (cl.->hex p.mantle)}) ; Screen-column at the cursor, when 'cursorcolumn' is set.
(set-hl :CursorLine        {:fg p.none :bg (cl.->hex p.surface0)}) ; Screen-line at the cursor, when 'cursorline' is set.

(set-hl :DiffAdd           {:fg (cl.->hex p.green)}) ; diff mode: Added line |diff.txt|
(set-hl :DiffChange        {:fg (cl.->hex p.blue)}) ; diff mode: Changed text within a changed line |diff.txt|
(set-hl :DiffDelete        {:fg (cl.->hex p.red)}) ; diff mode: Deleted line |diff.txt|
(set-hl :DiffText          {:fg (cl.->hex p.blue)}) ; diff mode: Changed text within a changed line |diff.txt|

(set-hl :Directory         {:fg (cl.->hex p.blue)}) ; directory names (and other special names in listings)
(set-hl :EndOfBuffer       {:fg (cl.->hex p.text) :bg (cl.->hex p.base)}) ; filler lines (~) after the end of the buffer.  By default, this is highlighted like |hl-NonText|.
(set-hl :ErrorMsg          {:fg (cl.->hex p.red) :bg p.none :bold true}) ; error messages on the command line
(set-hl :FoldColumn        {:fg (cl.->hex p.overlay0)}) ; column used for 'foldcolumn'
(set-hl :SignColumn        {:fg (cl.->hex p.surface1)}) ; column where signs are displayed
(set-hl :SignColumnSB      {:fg (cl.->hex p.surface1) :bg (cl.->hex p.crust)}) ; column where signs are displayed
(set-hl :Substitute        {:fg (cl.->hex p.pink) :bg (cl.->hex p.surface1)}) ; |:substitute| replacement text highlighting
(set-hl :LineNR            {:fg (cl.->hex p.surface1)}) ; Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
(set-hl :CursorLineNr      {:fg (cl.->hex p.lavender)}) ; Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
(set-hl :MatchParen        {:fg (cl.->hex p.peach) :bg (cl.->hex p.surface1) :bold true}) ; The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
(set-hl :ModeMsg           {:fg (cl.->hex p.text) :bg p.none :bold true}) ; 'showmode' message (e.g., "-- INSERT -- ")
(set-hl :MsgSeparator      {:fg p.none :bg p.none}) ; Separator for scrolled messages, `msgsep` flag of 'display'

(set-hl :MoreMsg           {:fg (cl.->hex p.blue)}) ; |more-prompt|
(set-hl :NonText           {:fg (cl.->hex p.overlay0)}) ; '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line).
(set-hl :Normal            {:fg (cl.->hex p.text) :bg (cl.->hex p.base)}) ; normal text
(set-hl :NormalNC          {:fg (cl.->hex p.text) :bg (cl.->hex p.base)}) ; normal text in non-current windows
(set-hl :NormalSB          {:fg (cl.->hex p.text) :bg (cl.->hex p.crust)}) ; normal text in non-current windows
(set-hl :NormalFloat       {:fg (cl.->hex p.text)}) ; normal text in floating windows

(set-hl :FloatBorder       {:fg (cl.->hex p.rosewater) :bg p.none})
(set-hl :FloatTitle        {:fg (cl.->hex p.subtext0)}) ; float window title
(set-hl :Pmenu             {:fg (cl.->hex p.overlay2) :bg (cl.->hex p.surface0)}) ; Popup menu: normal item.
(set-hl :PmenuSel          {:fg p.none :bg (cl.->hex p.surface1) :bold true}) ; Popup menu: selected item.
(set-hl :PmenuSbar         {:fg p.none :bg (cl.->hex p.surface1)}) ; Popup menu: scrollbar.
(set-hl :PmenuThumb        {:fg p.none :bg (cl.->hex p.overlay2)}) ; Popup menu: Thumb of the scrollbar.
(set-hl :Question          {:fg (cl.->hex p.blue)})
(set-hl :QuickFixLine      {:fg p.none :bg (cl.->hex p.surface1) :bold true}) ; Current |quickfix| item in the quickfix window. Combined with |hl-CursorLine| when the cursor is there.
;
(set-hl :SpecialKey           {:link :NonText}) ; Unprintable characters: text displayed differently from what it really is.  But not 'listchars' whitespace. |hl-Whitespace|
(set-hl :SpellBad             {:fg p.none :bg p.none :undercurl true}) ; Word that is not recognized by the spellchecker. |spell| Combined with the highlighting used otherwise.
(set-hl :SpellCap             {:fg p.none :bg p.none :undercurl true}) ; Word that should start with a capital. |spell| Combined with the highlighting used otherwise.
(set-hl :SpellLocal           {:fg p.none :bg p.none :undercurl true}) ; Word that is recognized by the spellchecker as one that is used in another region. |spell| Combined with the highlighting used otherwise.
(set-hl :SpellRare            {:fg p.none :bg p.none :undercurl true}) ; Word that is recognized by the spellchecker as one that is hardly ever used.  |spell| Combined with the highlighting used otherwise.
(set-hl :Statusline           {:fg (cl.->hex p.text)     :bg (cl.->hex p.base)}) ; status line of current window
(set-hl :StatusLineNC         {:fg (cl.->hex p.surface1) :bg (cl.->hex p.mantle)}) ; status lines of not-current windows
(set-hl :StatusLineTerm       {:link :StatusLine}) ; status line of current terminal window
(set-hl :StatusLineTermNC     {:link :StatusLineNC}) ; status lines of not-current terminal windows

(set-hl :TabLine           {:fg (cl.->hex p.surface1) :bg (cl.->hex p.base)}) ; tab pages line, not active tab page label
(set-hl :TabLineFill       {:fg p.none :bg p.none}) ; tab pages line, where there are no labels
(set-hl :TabLineSel        {:fg (cl.->hex p.green) :bg (cl.->hex p.surface1)}) ; tab pages line, active tab page label
(set-hl :Title             {:fg (cl.->hex p.blue) :bg p.none :bold true}) ; titles for output from ":set all", ":autocmd" etc.
(set-hl :VertSplit         {:fg (cl.->hex p.red) :bg (cl.->hex p.base)}) ; the column separating vertically split windows
(set-hl :Visual            {:fg p.none :bg (cl.->hex p.surface1) :bold true}) ; Visual mode selection
(set-hl :VisualNOS         {:fg p.none :bg (cl.->hex p.surface1) :bold true}) ; Visual mode selection when vim is "Not Owning the Selection".
(set-hl :WarningMsg        {:fg (cl.->hex p.yellow)}) ; warning messages
(set-hl :Whitespace        {:fg (cl.->hex p.surface1)}) ; "nbsp", "space", "tab" and "trail" in 'listchars'
(set-hl :WinBar            {:fg p.none :bg (cl.->hex p.base) :bold true})

(defn main [{: c : s : add-group}])
