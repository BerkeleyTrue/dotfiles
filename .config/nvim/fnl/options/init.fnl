(module options
  {autoload
   {r r
    utils utils
    ft options.fold-text
    auto options.auto
    aam options.auto-module}
   require-macros [macros]})

(defn main []
  (let [session-options (->>
                          (o sessionoptions)
                          (r.split ",")
                          (r.reject #(r.includes? [:terminal :blank] $))
                          (r.join ","))]
    (o! sessionoptions session-options))
  (o! ambiwidth      :single)                          ;  force East Asian Width Class chars into a single space
  (o! autoread       true)                             ;  autoread the file into buffer on focus
  (o! clipboard      :unnamedplus)                     ;  default yank into + register, which is the default clipboard for linux may break in osx?
  (o! cmdheight      2)                                ;  better display for messages
  (o! copyindent     true)                             ;  copy the previous indentation on autoindenting
  (o! completeopt    "menuone,noinsert,noselect")      ;  better completion options
  (o! cursorcolumn   true)                             ;  cursor column
  (o! cursorline     true)                             ;  cursorline
  (o! expandtab      true)                             ;  convert tabs to spaces
  (o! foldlevelstart 99)                               ;  start with all folds open
  (o! foldtext       ft.format-fold-text-viml)         ;  set fold text to custom format
  (o! ignorecase     true)                             ;  ignore case when searching
  (o! list           true)                             ;  set list mode for listchars
  (o! listchars      "tab:\\ ")                        ;  mark whitespace
  (o! mouse          :a)                               ;  enable the use of the mouse
  (o! backup         false)                            ;  disable backups"
  (o! swapfile       false)                            ;  disable backups"
  (o! wrap           false)                            ;  don't wrap lines
  (o! number         true)                             ;  hybrid mode numbers
  (o! shiftround     true)                             ;  use multiple of shiftwidth when indenting with "<" and ">"
  (o! shiftwidth     2)                                ;  number of spaces to use for autoindenting
  (o! shortmess      (.. (o shortmess) "c"))           ;  don't give |ins-completion-menu| messages.
  (o! showmatch      true)                             ;  set show matching parenthesis
  (o! signcolumn     :yes)                             ;  always show signcolumns
  (o! spell          true)                             ;  enable spell checking
  (o! spelllang      :en_us)                           ;  set spell language to US english
  (o! spelloptions   :camel)                           ;  ignore camel case words
  (o! nosmarttab     true)                             ;  don't use smart tabs
  (o! relativenumber false)                            ;  no relative numbers
  (o! synmaxcol      512)                              ;  prevent long lines from hanging vim
  (o! tabstop        2)                                ;  a tab is two spaces
  (o! termguicolors  true)                             ;  use gui colors in term's that support it
  (o! timeoutlen     300)                              ;  add mapping key timeout delay
  (o! title          true)                             ;  change the terminal"s title
  (o! ttimeoutlen    0)                                ;  remove key code delays
  (o! undolevels     1000)                             ;  use many muchos levels of undo
  (o! updatetime     300)                              ;  smaller updatetime for CursorHold & CursorHoldI
  (o! visualbell     true)                             ;  flash screen on error
  (o! wildignore     "*.swp,*.bak,*.pyc,*.class")      ;  ignore these files
  (o! winbar         ">=>%f")                          ;  default winbar, prevent FOUC
  (o! zip_unzipcmd   "7z x -so")                       ;  use 7z to unzip files

  ; nvim blinking cursor see :help 'guicursor'
  (o! guicursor "n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait200-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175")

  (command filetype :plugin :on)
  (command filetype :indent :on)
  (command syntax :on)
  (auto.main)
  (aam.main))
