(module plugins.defx
  {:require {nvim aniseed.nvim
             utils utils
             nutils aniseed.nvim.util
             a aniseed.core
             str aniseed.string}})


(nvim.fn.call
  :defx#custom#option ["_"
                       {:columns "indent:mark:git:icons:icon:filename"
                        :winwidth 32
                        :show_ignored_files 1
                        :direction "topleft"
                        :split "vertical"}])

(nvim.fn.call
  :defx#custom#column ["filename"
                       {:min_width 32
                        :max_width -90}])

(nvim.fn.call
  :defx#custom#column ["icon"
                       {:directory_icon "▸"
                        :opened_icon    "▾"
                        :root_icon      "."}])

(nvim.fn.call
  :defx#custom#column ["mark"
                       {
                        :readonly_icon "✗"
                        :selected_icon "✓"}])

(a.assoc nvim.g
         "defx_git#indicators"
         {
          :Deleted   "✖"
          :Ignored   "☒"
          :Modified  "✹"
          :Renamed   "➜"
          :Staged    "✚"
          :Unknown   "?"
          :Unmerged  "═"
          :Untracked "✭"})


(do
  (nvim.ex.highlight
    :Defx_filename_4_opened_icon :ctermfg=84)

  (nvim.ex.highlight
    :Defx_filename_4_directory_icon
    :ctermfg=117)

  ; git
  (nvim.ex.highlight :Defx_filename_4_Deleted :ctermfg=167 :guifg=#fb4934)
  (nvim.ex.highlight :Defx_filename_4_Ignored :guibg=NONE :guifg=NONE
                                              :ctermbg=NONE :ctermfg=NONE)
  (nvim.ex.highlight :Defx_filename_4_Modified :ctermfg=9 :guifg=#fabd2f)
  (nvim.ex.highlight :Defx_filename_4_Renamed :ctermfg=214 :guifg=#fabd2f)
  (nvim.ex.highlight :Defx_filename_4_Staged :ctermfg=142 :guifg=#b8bb26)
  (nvim.ex.highlight :Defx_filename_4_Unknown :guibg=NONE :guifg=NONE
                                              :ctermbg=NONE :ctermfg=NONE)
  (nvim.ex.highlight :Defx_filename_4_Unmerged :ctermfg=167 :guifg=#fb4934)
  (nvim.ex.highlight :Defx_filename_4_Untracked :guibg=NONE :guifg=NONE
                                                :ctermbg=NONE :ctermfg=NONE))

(defn is-defx-buf []
  (= (. nvim.bo :filetype) "defx"))

(defn defx-explorer [dir]
  ; open defx explorer
  (let [dir (if (str.blank? dir) (nvim.fn.getcwd) dir)
        is-defx (is-defx-buf)]

    (if is-defx
      (nvim.fn.call :defx#call_action ["quit"])
      (nvim.ex.Defx "\"-buffername=`'defx' . tabpagenr()`\"" dir))))

(nutils.fn-bridge "DefxExplorer" "plugins.defx" "defx-explorer")

(nvim.set_keymap
  "n"
  "get"
  ":call DefxExplorer(getcwd())<CR>"
  {:silent true
   :noremap true})

(defn defx-search [search dir]
  ; open defx and search for file in tree, expand that tree
  ; If already in a defx buffer, close it
  (let [dir (if (str.blank? dir) (nvim.fn.getcwd) dir)
        search (if (str.blank? search) (nvim.fn.expand "%:p") search)
        is-defx (is-defx-buf)]

    (if is-defx
      (nvim.fn.call :defx#call_action ["quit"])
      (nvim.ex.Defx (.. "-search=" search) "\"-buffername=`'defx' . tabpagenr()`\"" dir))))

(nutils.fn-bridge "DefxSearch" "plugins.defx" "defx-search")

(nvim.set_keymap
  "n"
  "gef"
  ":call DefxSearch(expand('%:p'), getcwd())<CR>"
  {:silent true
   :noremap true})

(defn defx-change-root []
  (let [is-dir (nvim.fn.call :defx#is_directory)]
    (when is-dir
      (do
        (nvim.fn.call :defx#call_action ["yank_path"])
        (nvim.fn.call :defx#call_action ["cd" (nvim.fn.getreg 0)])))))

(nutils.fn-bridge "DefxChangeRoot" "plugins.defx" "defx-change-root")

(defn nnoremap-buf-expr [lhs rhs]
  (utils.nnoremap lhs rhs {:buffer true :expr true :silent true}))

;; all are buffer
(defn defx-settings []
  ;; not expression
  (utils.nnoremap "cr"          ":call DefxChangeRoot()<CR>" {:buffer true :silent true :expr false})
  (nnoremap-buf-expr "<C-p>"    "defx#do_action('cd', ['..'])")
  (nnoremap-buf-expr "."        "defx#do_action('toggle_ignored_files')")

  (nnoremap-buf-expr "<Tab>"    "defx#do_action('toggle_select')")
  (nnoremap-buf-expr "<Space>" (..
                                 "defx#is_directory() ?"
                                 " defx#do_action('open_tree', 'toggle') :"
                                 " defx#do_action('close_tree')"))

  ;; all are expr
  (nnoremap-buf-expr "<CR>"    (..
                                "defx#is_directory() ?"
                                " defx#do_action('open_tree', 'toggle') :"
                                " defx#do_action('multi', ['drop', 'quit'])"))

  (nnoremap-buf-expr "<C-h>"    "defx#do_action('multi', [[ 'drop', 'split' ], 'quit'])")
  (nnoremap-buf-expr "<C-v>"    "defx#do_action('multi', [[ 'drop', 'vsplit' ], 'quit'])")
  (nnoremap-buf-expr "<C-t>"    "defx#do_action('multi', [[ 'drop', 'tabnew' ], 'quit'])")

  (nnoremap-buf-expr "L"        "defx#do_action('open_tree').'j'")
  (nnoremap-buf-expr "H"        "defx#do_action('close_tree')")

  (nnoremap-buf-expr "j"        "line('.') == line('$') ? 'gg' : 'j'")
  (nnoremap-buf-expr "k"        "line('.') == 1 ? 'G' : 'k'")

  (nnoremap-buf-expr "yp"       "defx#do_action('yank_path')")


  (nnoremap-buf-expr  "a"       "defx#do_action('new_file')")
  (nnoremap-buf-expr  "A"       "defx#do_action('new_directory')")

  (nnoremap-buf-expr  "C"       "defx#do_action('copy')")
  (nnoremap-buf-expr  "M"       "defx#do_action('move')")
  (nnoremap-buf-expr  "P"       "defx#do_action('paste')")
  (nnoremap-buf-expr  "r"       "defx#do_action('rename')")
  (nnoremap-buf-expr  "D"       "defx#do_action('remove')")

  (nnoremap-buf-expr  "R"       "defx#do_action('redraw')")
  (nnoremap-buf-expr  "cd"      "defx#do_action('change_vim_cwd')")

  (nnoremap-buf-expr  ">>"      "defx#do_action('resize', defx#get_context().winwidth + 20)")
  (nnoremap-buf-expr  "<<"      "defx#do_action('resize', defx#get_context().winwidth - 20)"))


(do
  (nvim.ex.augroup :defx-settings-au)
  (nvim.ex.autocmd_)
  (nvim.ex.autocmd (..
                     "FileType defx "
                     ":"
                     (utils.viml->lua
                       :plugins.defx
                       :defx-settings)))
  (nvim.ex.autocmd "FileType defx setlocal spell!")
  (nvim.ex.autocmd "VimResized defx call defx#call_action('resize', winwidth(0))")
  (nvim.ex.autocmd "BufWritePost * call defx#redraw()")
  (nvim.ex.augroup :END)
  {:defx-settings defx-settings
   :defx-search defx-search
   :defx-explorer defx-explorer})
