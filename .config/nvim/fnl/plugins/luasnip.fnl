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
   :gitcommit])

(def *lua-file*
  (utils.fn.substitute *file* "^fnl\\|fnl$" "lua" "g"))

(def *lua-dir*
  (utils.fn.substitute *lua-file* "\\.lua$" "/" ""))

(defn source-ft-snips []
  (when-let [luasnip (md.prequire :luasnip)]
    (let [fmts (md.prequire :luasnip.extras.fmt)
          methods {:fmt (. fmts :fmt)
                   :fmta (. fmts :fmta)
                   :s (. luasnip :snippet)

                   :i (. luasnip :insert_node)
                   :t (. luasnip :text_node)
                   :sn (. luasnip :snippet_node)
                   :c (. luasnip :choice_node)
                   :f (. luasnip :function_node)
                   :d (. luasnip :dynamice_node)}]
      (->>
        ft-snips
        (r.map
          (fn [ft]
            {: ft
             :namespace (.. *module-name* "." ft)}))

        (r.map
          (fn [{: ft : namespace}]
            {ft
             (md.prequire-main namespace methods)}))
        (r.reduce
          (fn [snippets ft-snips]
            (r.merge snippets ft-snips))
          {:all [(luasnip.parser.parse_snippet "test" "; tested")]})))))

(comment (source-ft-snips))

(defn source-snips []
  (anenv.compile-fnl)
  (utils.ex.source *lua-file*)
  (->>
    ft-snips
    (r.map #(.. *lua-dir* $ ".lua"))
    (r.for-each utils.ex.source))

  (when-let [snippets (source-ft-snips)]
    (let [luasnip (md.prequire :luasnip)]
      (set luasnip.snippets snippets))))

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
  (when-let [luasnip (md.packadd-n-require :luasnip)]
    (utils.ex.command_
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
         :enable_autosnippets false

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
      (set luasnip.snippets snippets))))