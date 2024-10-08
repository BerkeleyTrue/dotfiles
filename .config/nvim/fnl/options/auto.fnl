(module options.auto
  {require
   {a aniseed.core
    r r
    utils utils
    buffs lib.buffers}
   require-macros [macros]})

(defn go-to-last-edit []
  (when
    (and
      ; don't run in commit messages
      (not= (bo :filetype) :gitcommit)
      ; last mark is at '"
      ; so if the last mark is more than the first line (>0)
      ; and less then the files last line ("$")
      (and
        (> (vim.fn.line "'\"") 0)
        (<= (vim.fn.line "'\"") (vim.fn.line "$"))))

    ; g'"|g`"" Jump to the last known position, but don't change the jumplist
    ; zv open folds enough to view cursor
    ; zz center cursor line on screen
    (command normal! "g`\"zvzz")))

(defn hpack-auto-gen []
  (let [res (vim.fn.system (.. "hpack " (vim.fn.expand "%")))]
    (if (not= (v shell_error) 0)
      (echoerr res)
      (print res))))

(defn main []
  (augroup
    :GeneralAu

    ; Resize splits when the vim is resized
    ; Also make sure the help window is always 81 columns wide
    {:event :VimResized
     :pattern :*
     :callback
     (fn [] 
       (let [buffers (buffs.list-visible)
             help-winnr (buffs.get-help-window)]
         (when (> (length buffers) 1)
           (command wincmd :=))
         (when help-winnr
           (n win-set-width help-winnr 81))))}

    ; Make vim open on the line you closed the buffer on
    {:event [:BufReadPost]
     :pattern :*
     :callback go-to-last-edit}

    ; Make vim open help buffers in a vertical split
    {:event :FileType
     :pattern :help
     :cmd "wincmd L | vert resize 81"}

    {:event [:BufEnter :BufReadPost] 
     :pattern :*
     :callback 
     #(when (= (o filetype) :help)
        (vim.fn.buflisted "help")
        (command vertical "resize 100"))}

    {:event :BufLeave
     :pattern :*
     :callback 
     #(when (= (o filetype) :help)
        (command vertical "resize 81"))}

    ; make sure cursor always starts on the first line for gitcommit files
    {:event [:FileType]
     :pattern :gitcommit
     :cmd "call setpos ('.', [0, 1, 1, 0])"}

    {:event [:BufWritePost]
     :pattern :package.yaml
     :callback hpack-auto-gen}
    
    {:event :TextYankPost
     :pattern :*
     :cmd "silent! lua vim.highlight.on_yank{timeout=300}"}))
