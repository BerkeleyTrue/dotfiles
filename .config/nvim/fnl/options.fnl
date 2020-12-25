(module options
  {:require {: utils}})

(let [shortmess (.. (. utils.o :shortmess) "c")]
  (->
    {
     :ambiwidth      :single                          ;  force East Asian Width Class chars into a single space
     :autoread       true                             ;  autoread the file into buffer on focus
     :clipboard      :unnamedplus                     ;  default yank into + register, which is the default clipboard for linux may break in osx?
     :cmdheight      2                                ;  better display for messages
     :copyindent     true                             ;  copy the previous indentation on autoindenting
     :cursorcolumn   true                             ;  cursor column
     :cursorline     true                             ;  cursorline
     :expandtab      true                             ;  convert tabs to spaces
     :ignorecase     true                             ;  ignore case when searching
     :list           true                             ;  set list mode for listchars
     :listchars      "tab:>."                         ;  mark whitespace
     :mouse          :a                               ;  enable the use of the mouse
     :backup         false                            ;  disable backups"
     :swapfile       false                            ;  disable backups"
     :wrap           false                            ;  don't wrap lines
     :number         true                             ;  hybrid mode numbers
     :shiftround     true                             ;  use multiple of shiftwidth when indenting with "<" and ">"
     :shiftwidth     2                                ;  number of spaces to use for autoindenting
     :shortmess      shortmess                        ;  don't give |ins-completion-menu| messages.
     :showmatch      true                             ;  set show matching parenthesis
     :signcolumn     :yes                             ;  always show signcolumns
     :spell          true                             ;  enable spell checking
     :spelllang      :en_us                           ;  set spell language to US english
     :relativenumber false                            ;  no relative numbers
     :synmaxcol      512                              ;  prevent long lines from hanging vim
     :tabstop        2                                ;  a tab is two spaces
     :termguicolors  true                             ;  use gui colors in term's that support it
     :timeoutlen     1000                             ;  add mapping key timeout delay
     :title          true                             ;  change the terminal"s title
     :ttimeoutlen    0                                ;  remove key code delays
     :undolevels     1000                             ;  use many muchos levels of undo
     :updatetime     300                              ;  smaller updatetime for CursorHold & CursorHoldI
     :visualbell     true                             ;  flash screen on error
     :wildignore     "*.swp,*.bak,*.pyc,*.class"      ;  ignore these files

     ; nvim blinking cursor see :help 'guicursor'
     :guicursor  "n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait200-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175"}
    (utils.set-nvim-o!)))
(utils.ex.filetype :plugin :on)
(utils.ex.filetype :indent :on)
