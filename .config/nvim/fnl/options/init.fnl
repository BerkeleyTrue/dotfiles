(module options
  {require
   {r r
    utils utils
    auto options.auto
    aam options.auto-module}
   require-macros [macros]})


; parinfer does not like this file
(defn format-fold-text []
  (let [foldstart (v :foldstart)
        foldend (v :foldend)
        numcolwidth (+
                      (tonumber (. utils.wo :foldcolumn))
                      (* (if (. utils.wo :number) 1 0)
                         (. utils.wo :numberwidth)))
        windowwidth (- (utils.fn.winwidth 0) numcolwidth 3)
        lines-to-fold (- foldend foldstart)

        onetab (utils.fn.strpart "          " 0 (. utils.o :tabstop))]
    (->
      ; get start of fold
      foldstart
      ; get whole line of fold
      (utils.fn.getline)
      ; substitute tabs into spaces
      (utils.fn.substitute :\t onetab :g)
      ; grab the first n chars of the line
      ; n is the windowwidth - the width of the num of lines - magic number
      (utils.fn.strpart 0 (- windowwidth (utils.fn.len lines-to-fold) 5))
      (.. "..." lines-to-fold " lines folded...>>>"))))

(def format-fold-text-viml (.. (utils.viml-fn-bridge *module-name* (sym->name format-fold-text)) "()"))

(let [shortmess (.. (. utils.o :shortmess) "c")]
  (->
    {
     :ambiwidth      :single                          ;  force East Asian Width Class chars into a single space
     :autoread       true                             ;  autoread the file into buffer on focus
     :clipboard      :unnamedplus                     ;  default yank into + register, which is the default clipboard for linux may break in osx?
     :cmdheight      2                                ;  better display for messages
     :copyindent     true                             ;  copy the previous indentation on autoindenting
     :completeopt    "menuone,noinsert,noselect"
     :cursorcolumn   true                             ;  cursor column
     :cursorline     true                             ;  cursorline
     :expandtab      true                             ;  convert tabs to spaces
     :ignorecase     true                             ;  ignore case when searching
     :list           true                             ;  set list mode for listchars
     :listchars      "tab:\\ \\ "                     ;  mark whitespace
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
     :spelloptions   :camel
     :nosmarttab     true
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
     :winbar         ">=>%f"                          ;  default winbar, prevent FOUC

     ; nvim blinking cursor see :help 'guicursor'
     :guicursor  "n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait200-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175"
     :foldtext     format-fold-text-viml}
    (utils.set-nvim-o!)))

(g! loaded_matchit true)
(command filetype :plugin :on)
(command filetype :indent :on)
(command syntax :on)
(auto.main)
(aam.main)
