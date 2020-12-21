(module plugins.defx
  {:require {: utils
             : r}
   :require-macros [macros]})

(r.assoc
  utils.g
  :defx_git#indicators
  {:Deleted   "✖"
   :Ignored   "☒"
   :Modified  "✹"
   :Renamed   "➜"
   :Staged    "✚"
   :Unknown   "?"
   :Unmerged  "═"
   :Untracked "✭"})

(do
  (utils.hi-link! :DefxIconsOpenedTreeIcon :BerksGreen)
  (utils.hi-link! :DefxIconsDirectory :BerksCyan)
  (utils.hi-link! :DefxIconsParentDirectory :BerksCyan)
  (utils.hi-link! :Defx_icons_js :BerksGreen)
  (utils.hi-link! :Defx_icons_vim :BerksPink)
  (utils.hi-link! :Defx_icons_markdown :BerksRed)
  (utils.hi-link! :Defx_icons_fnl :BerksRed)

  ; git
  (utils.hi-link! :Defx_git_Deleted :BerksRed)
  (utils.hi-link! :Defx_git_Ignored :BerksNone)
  (utils.hi-link! :Defx_git_Modified :BerksPink)
  (utils.hi-link! :Defx_git_Renamed :BerksOrange)
  (utils.hi-link! :Defx_git_Staged :BerksPurple)
  (utils.hi-link! :Defx_git_Unknown :BerksNone)
  (utils.hi-link! :Defx_git_Unmerged :BerksRed)
  (utils.hi-link! :Defx_git_Untracked :BerksNone))

(defn- is-defx-buf []
  (= (. utils.bo :filetype) "defx"))

(defn defx-explorer [dir]
  ; open defx explorer
  (let [dir (if (r.empty? dir) (utils.fn.getcwd) dir)
        is-defx (is-defx-buf)]

    (if is-defx
      (utils.fn.call :defx#call_action ["quit"])
      (utils.ex.Defx "\"-buffername=`'defx' . tabpagenr()`\"" dir))))

(defn defx-search [search dir]
  ; open defx and search for file in tree, expand that tree
  ; If already in a defx buffer, close it
  (let [dir (if (r.empty? dir) (utils.fn.getcwd) dir)
        search (: (if (r.empty? search) (utils.fn.expand "%:p") search) :gsub " " "\\ ")
        is-defx (is-defx-buf)]

    (if is-defx
      (utils.fn.call :defx#call_action ["quit"])
      (utils.ex.Defx (.. "-search=" search) "\"-buffername=`'defx' . tabpagenr()`\"" dir))))

(defn defx-change-root []
  (let [is-dir (utils.fn.call :defx#is_directory)]
    (when is-dir
      (do
        (utils.fn.call :defx#call_action ["yank_path"])
        (utils.fn.call :defx#call_action ["cd" (utils.fn.getreg 0)])))))

(defn- nnoremap-buf-expr [lhs rhs]
  (utils.nnoremap lhs rhs {:buffer true :expr true :silent true}))

; settings while in defx buffer
(defn defx-settings []
  ;; not expression
  (utils.nnoremap "cr"          (utils.cviml->lua *module-name* (sym->name defx-change-root)) {:buffer true :silent true})
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

(defn main []
  (let [ok (pcall utils.ex.packadd :defx.nvim)]
    (if
      (not ok) (print (.. "could not load defx vim"))

      (do
        (utils.fn.call
          :defx#custom#option
          ["_"
           {:columns            "indent:mark:git:icons:icon:filename"
             :winwidth           32
             :show_ignored_files 1
             :direction          "topleft"
             :split              "vertical"}])

        (utils.fn.call
          :defx#custom#column
          ["filename"
           {:min_width 32
             :max_width -90}])

        (utils.fn.call
          :defx#custom#column
          ["icon"
           {:directory_icon "▸"
             :opened_icon    "▾"
             :root_icon      "."}])

        (utils.fn.call
          :defx#custom#column
          ["mark"
           {:readonly_icon "✗"
             :selected_icon "✓"}])

        (utils.nnoremap
          "get"
          (utils.cviml->lua *module-name* (sym->name defx-explorer))
          {:silent true})

        (utils.nnoremap
          "gef"
          (utils.cviml->lua *module-name* (sym->name defx-search))
          {:silent true})

        (utils.augroup
          :defx-settings-au
          [{:event :FileType
            :pattern :defx
            :cmd (.. ":" (utils.viml->lua *module-name* (sym->name defx-settings)))}
           {:event :FileType
            :pattern :defx
            :cmd "setlocal nospell"}
           {:event :VimResized
            :pattern :defx
            :cmd "call defx#call_action('resize', winwidth(0))"}
           {:event :BufWritePost
            :pattern :*
            :cmd "call defx#redraw()"}])))))
