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

(defn- is-empty-file []
  "Check if the current file is empty."
  (and
    ; only run if the file is empty
    (= (vim.fn.line "$") 1) ;; last line is also the first line
    (= (vim.fn.getline 1) ""))) ;; first line is empty

(defn- is-file-type [filetype]
  "check if the current file is of a certain type"
  (= (bo :filetype) filetype))

(defn auto-add-fennel-module []
  (when (and (is-file-type :fennel) (is-empty-file))
    ; get the relative file path of the current file
    (let [file-path (vim.fn.expand "%:p")
          fnldir (.. (vim.fn.stdpath :config) "/fnl/")
          fnltestdir (.. (vim.fn.stdpath :config) "/test/fnl/")
          in-test-dir (not= (vf stridx file-path fnltestdir) -1)
          ; is file path in fnl directory
          in-fnl-dir (not= (vf stridx file-path fnldir) -1)]

      (when (or in-test-dir in-fnl-dir)
        ; get the path of the file relative to the fnl directory
        (let [mname (->
                      file-path
                      (vim.fn.substitute fnldir "" "")
                      (vim.fn.substitute fnltestdir "" "")
                      (vim.fn.substitute "\\init.fnl$" "" "")
                      (vim.fn.substitute "\\.fnl$" "" "")
                      (vim.fn.split "/")
                      (vim.fn.join "."))]
          ; insert the module at the top of the file
          (vim.fn.append 0
            [(.. "(module " mname)
             "  {autoload"
             "   {a aniseed.core"
             "    r r"
             "    md utils.module"
             "    utils utils}"
             "   require {}"
             "   require-macros [macros]})"]))))))

(defn auto-add-purescript-module []
  (when (and (is-file-type :purescript) (is-empty-file))
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
  (when (and (is-file-type :haskell)  (is-empty-file))
    ; get the relative file path of the current file
    (let [file-path (vim.fn.expand "%:p")
          rootdir (vim.fn.finddir :.git/.. (.. (vim.fn.expand "%:p:h") ";"))
          rootdir (if (not= rootdir "")
                    rootdir
                    (vim.fn.finddir :.stack-work/.. (.. (vim.fn.expand "%:p:h") ";")))]

      ; get the path of the file relative to the rootdir directory
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

(comment
  ; get the name of the current file folder
  (vim.fn.expand "%:p:h:t"))

(defn auto-add-go-package []
  (when (and (or (is-file-type :go) (is-file-type :templ)) (is-empty-file))
    ; get the relative file path of the current file
    (let [name (vim.fn.expand "%:p:h:t")]
      ; insert the module at the top of the file
      (vim.fn.append 0 [(.. "package " name)]))))

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
     :callback auto-add-haskell-module}

    {:event [:Filetype]
     :pattern "go"
     :callback auto-add-go-package}

    {:event [:Filetype]
     :pattern "templ"
     :callback auto-add-go-package}))
