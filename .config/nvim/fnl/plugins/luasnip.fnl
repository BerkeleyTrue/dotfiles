(module plugins.luasnip
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    anenv plugins.aniseed}
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
      (. spec :fts))
    spec))


(defn- normalize-spec [spec]
  (let [ft (if (= (type spec) "string")
             spec
             (. spec :ft))

        namespace (if (= (type spec) "string")
                    (.. *module-name* "." ft)
                    (.. *module-name* "." (. spec :ns)))]
    {: ft
     : namespace}))

(defn source-ft-snips []
  (when-let [luasnip (md.prequire :luasnip)]
    (let [fmts (md.prequire :luasnip.extras.fmt)
          xtras (md.prequire :luasnip.extras)
          ai (md.prequire :luasnip.nodes.absolute_indexer)
          methods {:fmt (. fmts :fmt)
                   :fmta (. fmts :fmta)
                   :rep (. xtras :rep)
                   :s (. luasnip :snippet)

                   :i (. luasnip :insert_node)
                   :t (. luasnip :text_node)
                   :sn (. luasnip :snippet_node)
                   :c (. luasnip :choice_node)
                   :f (. luasnip :function_node)
                   :d (. luasnip :dynamice_node)
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
            (let [pSnippets (or (. snippets ft) [])]
              ; merge new and prev snips, then map to ft, then merge with old map
              (r.merge snippets {ft (r.concat pSnippets snips)})))
          {:all [(luasnip.parser.parse_snippet "test" "; tested")]})))))

(comment (source-ft-snips))

(defn source-snips []
  "Compile fennel files, source this file in lua, for each filtype file,
  map to the lua version and source file"
  (anenv.compile-fnl)
  (utils.ex.source *lua-file*)
  (->>
    ft-snips
    (r.map #(.. *lua-dir* $ ".lua"))
    (r.for-each utils.ex.source))

  (when-let [snippets (source-ft-snips)]
    (let [luasnip (md.prequire :luasnip)]
      (luasnip.add_snippets nil snippets))))

(defn expand-or-jump []
  (when-let [ls (md.prequire :luasnip)]
    (when (ls.expand_or_jumpable)
      (ls.expand_or_jump)
      nil)))

(defn jump-back []
  (when-let [ls (md.prequire :luasnip)]
    (when (ls.jumpable -1)
      (ls.jump -1)
      nil)))

(defn switch-choice []
  (when-let [ls (md.prequire :luasnip)]
    (when (ls.choice_active)
      (ls.change_choice 1))))

(defn switch-choice-r []
  (when-let [ls (md.prequire :luasnip)]
    (when (ls.choice_active)
      (ls.change_choice -1))))

(defn main []
  (when-let [luasnip (md.prequire :luasnip)]
    (command!
      :SourceSnips
      (viml->lua* source-snips))

    (utils.inoremap
      :<C-j> (cviml->lua* expand-or-jump) {:silent true})
    (utils.snoremap
      :<C-j> (cviml->lua* expand-or-jump) {:silent true})

    (utils.inoremap
      :<C-k> (cviml->lua* jump-back) {:silent true})
    (utils.snoremap
      :<C-k> (cviml->lua* jump-back) {:silent true})

    (utils.imap
      :<C-l> (cviml->lua* switch-choice))

    (utils.imap
      :<C-h> (cviml->lua* switch-choice-r))

    (let [types (md.prequire :luasnip.util.types)]
      (luasnip.config.set_config
        {:history true
         :update_events "TextChanged,TextChangedI"
         :delete_check_events "InsertLeave"
         :enable_autosnippets true

         :ext_opts
         {types.choiceNode
          {:active
           {:virt_text
            [["🌔" :BerksCyan]]}}
          types.insertNode
          {:active
           {:virt_text
            [["🌖" :BerksGreen]]}}}}))
    (when-let [snippets (source-ft-snips)]
      (luasnip.add_snippets nil snippets))))
