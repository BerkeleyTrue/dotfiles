(module options.auto-module
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn capitalize-word [word]
  (vim.fn.substitute word "\\<." "\\u&" ""))

(defn captialize-words [words]
  (r.map capitalize-word words))

(comment
  (capitalize-word "hello")
  (captialize-words ["hello" "world"]))

(defn auto-add-fennel-module []
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

(defn auto-add-purescript-module []
  (when (and
          (= (bo :filetype) :purescript)
          ; only run if the file is empty
          (= (vim.fn.line "$") 1) ;; last line is also the first line
          (= (vim.fn.getline 1) "")) ;; first line is empty

    ; get the relative file path of the current file
    (let [file-path (vim.fn.expand "%:p")
          rootdir (vim.fn.finddir :.git/.. (.. (vim.fn.expand "%:p:h") ";"))]

      ; get the path of the file relative to the rootdir directory
      (when (not= rootdir "")
        (let [mname (->
                      file-path
                      (vim.fn.substitute rootdir "" "")
                      (vim.fn.substitute "src" "" "") ;; how to make general to all projects?
                      (vim.fn.substitute "\\.purs" "" "")
                      (vim.fn.split "/")
                      ; uppercase the first letter of each word
                      (captialize-words)
                      (vim.fn.join "."))]
          ; insert the module at the top of the file
          (vim.fn.append 0
            [(.. "module " mname " where")
             ""
             "import Prelude"]))))))

(defn auto-add-haskell-module []
  (when (and
          (= (bo :filetype) :haskell)
          ; only run if the file is empty
          (= (vim.fn.line "$") 1) ;; last line is also the first line
          (= (vim.fn.getline 1) "")) ;; first line is empty

    ; get the relative file path of the current file
    (let [file-path (vim.fn.expand "%:p")
          rootdir (vim.fn.finddir :.git/.. (.. (vim.fn.expand "%:p:h") ";"))
          rootdir (if (not= rootdir "")
                    rootdir
                    (vim.fn.finddir :.stack-work/.. (.. (vim.fn.expand "%:p:h") ";")))]

      ; get the path of the file relative to the rootdir directory
      (print (.. "rootdir: " rootdir))
      (if (not= rootdir "")
        (let [mname (->
                      file-path
                      (vim.fn.substitute rootdir "" "")
                      (vim.fn.substitute "src" "" "") ;; how to make general to all projects?
                      (vim.fn.substitute "\\.hs" "" "")
                      (vim.fn.split "/")
                      ; uppercase the first letter of each word
                      (captialize-words)
                      (vim.fn.join "."))]
          ; insert the module at the top of the file
          (vim.fn.append 0
            [(.. "module " mname " where")
             ""
             "import Prelude"]))
        :else (vim.fn.echo "No root directory found")))))

(defn main []
  ; add module to the top of the file when you open it if it's empty
  (augroup
    :AutoModuleDef
    {:event [:Filetype]
     :pattern "fennel"
     :callback auto-add-fennel-module}

    {:event [:Filetype]
     :pattern "purescript"
     :callback auto-add-purescript-module}

    {:event [:Filetype]
     :pattern "haskell"
     :callback auto-add-haskell-module}))
