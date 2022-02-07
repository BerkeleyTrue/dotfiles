(module options.auto
  {require {: utils}
   require-macros [macros]})

(defn disable-camel-case-spell []
  (utils.ex.syntax "match CamelCase /\\<[A-Z][a-z]\\+[A-Z].\\{-}\\>/ contains=@NoSpell transparent")
  (utils.ex.syntax "cluster Spell add=CamelCase"))

(defn go-to-last-edit []
  (when
    (and
      ; don't run in commit messages
      (not= (. utils.bo :filetype) :gitcommit)
      ; last mark is at '"
      ; so if the last mark is more than the first line (>0)
      ; and less then the files last line ("$")
      (and
        (> (utils.fn.line "'\"") 0)
        (<= (utils.fn.line "'\"") (utils.fn.line "$"))))
    ; g'"|g`"" Jump to the last known position, but don't change the jumplist
    ; zv open folds enough to view cursor
    ; zz center cursor line on screen
    (utils.ex.normal_ "g`\"zvzz")))

(utils.augroup
  :general-au
  [{:event [:BufEnter :BufReadPost]
      :pattern :*
      :cmd (utils.viml->lua *module-name* (sym->name disable-camel-case-spell))}
    ; Resize splits when the window is resized
    {:event :VimResized
      :pattern :*
      :cmd "exe \"normal! \\<c-w>=\""}

    ; Make vim open on the line you closed the buffer on
    {:event :BufReadPost
      :pattern :*
      :cmd (utils.viml->lua *module-name* (sym->name go-to-last-edit))}

    ; remove highlight after cursor stops moving
    {:event :CursorHold
      :pattern :*
      :cmd "set nohlsearch | let @/=''"}
    {:event :CursorMoved
      :pattern :* :cmd "set hlsearch"}])
