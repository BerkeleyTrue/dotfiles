(module plugins.luasnip
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    anenv plugins.aniseed}
   require-macros [macros]})

(def- ft-snips
  [:fennel])

(def *lua-file*
  (utils.fn.substitute *file* "^fnl\\|fnl$" "lua" "g"))

(def *lua-dir*
  (utils.fn.substitute *lua-file* "\\.lua$" "/" ""))

(defn source-ft-snips []
  (when-let [luasnip (md.prequire :luasnip)]
    (let [fmts (md.prequire :luasnip.extras.fmt)
          methods {:fmt (. fmts :fmt)
                   :fmta (. fmts :fmta)
                   :i (. luasnip :insert_node)
                   :s (. luasnip :snippet)}]
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
  ; TODO: resource all ft files using *lua-file*
  (->>
    ft-snips
    (r.map #(.. *lua-dir* $ ".lua"))
    (r.for-each utils.ex.source))

  (when-let [snippets (source-ft-snips)]
    (let [luasnip (md.prequire :luasnip)]
      (set luasnip.snippets snippets))))


(defn main []
  (when-let [luasnip (md.packadd-n-require :luasnip)]
    (utils.ex.command_
      :SourceSnips
      (viml->lua* source-snips))

    (let [types (md.prequire :luasnip.util.types)]
      (luasnip.config.set_config
        {:history true
         :updateevents "TextChanged,TextChangedI"
         :enable_autosnippets true
         :ext_opts
         {types.choiceNode
          {:active
           {:virtual_text
            [{:<- :Error}]}}}}))
    (when-let [snippets (source-ft-snips)]
      (set luasnip.snippets snippets))))
