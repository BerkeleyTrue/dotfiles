(module options.auto
  {require
   {a aniseed.core
    r r
    utils utils}
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

(defn auto-add-module []
  (when (and
          ; only run in fennel files
          (= (bo :filetype) :fennel)
          ; only run if the file is empty
          (= (vim.fn.line "$") 1) ;; last line is also the first line
          (= (vim.fn.getline 1) "")) ;; first line is empty

    ; get the relative file path of the current file
    (let [file-path (vim.fn.expand "%:p")
          fnldir (.. (vim.fn.stdpath :config) "/fnl/")
          ; is file path in fnl directory
          in-fnl-dir (not= (vim.fn.stridx file-path fnldir) -1)]

      (when in-fnl-dir
        ; get the path of the file relative to the fnl directory
        (let [mname (->
                      file-path
                      (vim.fn.substitute fnldir "" "")
                      (vim.fn.substitute "\\init.fnl$" "" "")
                      (vim.fn.substitute "\\.fnl$" "" "")
                      (vim.fn.split "/")
                      (vim.fn.join "."))]
          ; insert the module at the top of the file
          (vim.fn.append 0
            [(.. "(module " mname)
             "  {require"
             "   {a aniseed.core"
             "    r r"
             "    md utils.module"
             "    utils utils}"
             "   require-macros [macros]})"]))))))

(defn hpack-auto-gen []
  (let [res (vim.fn.system (.. "hpack " (vim.fn.expand "%")))]
    (if (not= (v shell_error) 0)
      (echoerr res)
      (print res))))

(augroup
  :GeneralAu

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
   :cmd "call setpos ('.', [0, 1, 1, 0])"}

  ; add module to the top of the file when you open it if it's empty
  {:event [:FileType]
   :pattern :fennel
   :callback auto-add-module}

  {:event [:BufWritePost]
   :pattern :package.yaml
   :callback hpack-auto-gen})
