(module plugins.luasnip
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    anenv plugins.aniseed

    ls luasnip
    xtras luasnip.extras
    fmts luasnip.extras.fmt
    ai luasnip.nodes.absolute_indexer
    types luasnip.util.types}
   require-macros [macros]})

(def- ft-snips
  [:fennel
   :gitcommit
   {:ns :typescript.react
    :fts [:typescriptreact]}
   ; spec format for shared config
   ; ns is the file to load
   ; fts are the filetypes this spec applies to
   {:fts [:javascript :typescript :typescriptreact]
    :ns :javascript.common}])

(def *lua-file*
  (.. (utils.fn.stdpath "config") "/" (utils.fn.substitute *file* "^fnl\\|fnl$" "lua" "g")))

(def *lua-dir*
  (utils.fn.substitute *lua-file* "\\.lua$" "/" ""))


(defn- map-fts [spec]
  "Allow common specs to spread to multiple filetypes"
  (if (r.table? spec)
    (a.map
      (fn [ft]
        (r.merge {: ft} spec))
      spec.fts)
    spec))


(defn- normalize-spec [spec]
  (let [ft (if (= (type spec) "string")
             spec
             spec.ft)

        namespace (if (= (type spec) "string")
                    (.. *module-name* "." ft)
                    (.. *module-name* "." (. spec :ns)))]
    {: ft
     : namespace}))

(defn source-ft-snips []
   (let [methods {:fmt fmts.fmt
                  :fmta fmts.fmta
                  :rep xtras.rep
                  :s ls.snippet

                  :i ls.insert_node
                  :t ls.text_node
                  :sn ls.snippet_node
                  :c ls.choice_node
                  :f ls.function_node
                  :d ls.dynamice_node
                  : ai}]
     (->>
       ft-snips
       (r.flatMap map-fts)
       (r.map normalize-spec)
       (r.map
         (fn [{: ft : namespace}]
           {: ft
            :snips (md.prequire-main namespace methods)}))
       (r.reduce
         (fn [snippets {: ft : snips}]
           ; pull already merged snippets
           (let [pSnippets (or snippets.ft [])]
             ; merge new and prev snips, then map to ft, then merge with old map
             (r.merge snippets {ft (r.concat pSnippets snips)})))
         {:all [(ls.parser.parse_snippet "test" "; tested")]}))))

(comment (source-ft-snips))

(defn source-snips []
  "Compile fennel files, source this file in lua, for each filtype file,
  map to the lua version and source file"
  (anenv.compile-fnl)
  (command source *lua-file*)
  (->>
    ft-snips
    (r.map #(.. *lua-dir* $ ".lua"))
    (r.for-each utils.ex.source))

  (when-let [snippets (source-ft-snips)]
    (ls.add_snippets nil snippets)))

(defn expand-or-jump []
  (when (ls.expand_or_jumpable)
    (ls.expand_or_jump)
    nil))

(defn jump-back []
  (when (ls.jumpable -1)
    (ls.jump -1)
    nil))

(defn switch-choice [dir]
  (when (ls.choice_active)
    (ls.change_choice (if dir -1 1))))

(defn init []
  (command! :SourceSnips source-snips {:desc "Source snippets"})

  (inoremap :<M-l> expand-or-jump {:silent true :desc "Luasnip: expand or jump to next section"})
  (snoremap :<M-l> expand-or-jump {:silent true :desc "Luasnip: expand or jump to next section"})

  (inoremap :<M-h> jump-back {:silent true :desc "Luasnip: jump to previous section"})
  (snoremap :<M-h> jump-back {:silent true :desc "Luasnip: jump to previous section"})

  (imap :<M-j> #(switch-choice false) {:desc "Luasnip: switch to next choice"})
  (imap :<M-k> #(switch-choice true) {:desc "Luasnip: switch to previous choice"}))

(defn main []
  (ls.config.set_config
    {:history true
     :update_events [:TextChanged :TextChangedI]
     :delete_check_events [:TextChanged :InsertLeave]
     :region_check_events [:CursorMoved]
     :enable_autosnippets true

     :ext_opts {types.choiceNode
                {:active
                  {:virt_text
                    [["🌔" :BerksSapphire]]}}
                types.insertNode
                {:active
                  {:virt_text
                    [["🌖" :BerksGreen]]}}}})
  (when-let [snippets (source-ft-snips)]
    (ls.add_snippets nil snippets)))
