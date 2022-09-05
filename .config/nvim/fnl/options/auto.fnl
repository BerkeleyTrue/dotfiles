(module options.auto
  {require
   {utils utils}
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
        (> (vim.fn.line "'\"") 0)
        (<= (vim.fn.line "'\"") (vim.fn.line "$"))))
    ; g'"|g`"" Jump to the last known position, but don't change the jumplist
    ; zv open folds enough to view cursor
    ; zz center cursor line on screen
    (utils.ex.normal_ "g`\"zvzz")))

(augroup
  :GeneralAu

  {:event [:BufEnter :BufReadPost :WinEnter]
   :pattern :*
   :callback disable-camel-case-spell}

  ; Resize splits when the window is resized
  {:event :VimResized
   :pattern :*
   :cmd "exe \"normal! \\<c-w>=\""}

  ; Make vim open on the line you closed the buffer on
  {:event [:BufReadPost]
   :pattern :*
   :callback go-to-last-edit}
  ; make sure cursor always starts on the first line for gitcommit files
  {:event [:FileType]
   :pattern :gitcommit
   :cmd "call setpos ('.', [0, 1, 1, 0])"})
