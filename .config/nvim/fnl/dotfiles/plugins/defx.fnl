(module dotfiles.plugins.defx
  {:require {nvim aniseed.nvim
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

(defn is-defx-buf [] (= (. nvim.o :filetype) "defx"))

(defn defx-explorer [dir]
  (let [dir (if (str.blank? dir) (nvim.fn.getcwd) dir)
        is-defx (is-defx-buf)]

    (if is-defx
      (nvim.fn.call :defx#call_action ["quit"])
      (nvim.ex.Defx ["-buffername=`'defx' . tabpagenr()`" dir]))))

(nutils.fn-bridge "DefxExplorer" "dotfiles.plugins.defx" "defx-explorer")

(nvim.set_keymap
  "n"
  "get"
  ":call DefxExplorer('')<CR>"
  {:silent true
   :noremap true})

(defn defx-search [search dir]
  (let [dir (if (str.blank? dir) (nvim.fn.getcwd) dir)
        search (if (str.blank? search) (nvim.fn.expand "%:p") search)
        is-defx (is-defx-buf)]

    (if is-defx
      (nvim.fn.call :defx#call_action ["quit"])
      (nvim.ex.Defx [(.. "-search=" search) "-buffername=`'defx' . tabpagenr()`" dir]))))

(nutils.fn-bridge "DefxSearch" "dotfiles.plugins.defx" "defx-search")

(nvim.set_keymap
  "n"
  "zef"
  ":call DefxSearch(expand('%:p'), getcwd())<CR>"
  {:silent true
   :noremap true})
