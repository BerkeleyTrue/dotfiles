(module plugins.comments
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def- jsxtypes
  [:typescriptreact
   :javascript
   :javascript.jsx])

(defn- pre-hook [ctx]
  "Calculates commentstring using ts_context_commentstring for jsx filetypes."
  (when (r.includes jsxtypes vim.bo.filetype)
    (let [cutils (md.prequire :Comment.utils)
          csinternal (md.prequire :ts_context_commentstring.internal)
          csutils (md.prequire :ts_context_commentstring.utils)
          ctype (or (and
                      (= ctx.ctype cutils.ctype.line)
                      :__default)
                    :__multiline)
          location (if (= ctx.ctype cutils.ctype.block)
                     (csutils.get_cursor_location)

                     (or (= ctx.cmotion cutils.cmotion.v)
                         (= ctx.cmotion cutils.cmotion.V))
                     (csutils.get_visual_start_location))]

      (csinternal.calculate_commentstring
        {:key ctype
         : location}))))

(def- configs
  {:padding true
   :sticky true
   :ignore nil

   :toggler
   {:line :gcc
    :block :gbc}

   :opleader
   {:line :gc
    :block :gb}

   :extra
   {:above :gcO
    :below :gco
    :eol :gcA}

   :mappings
   {:basic false
    :extra false
    :extended false}
   :pre_hook pre-hook
   :post_hook nil})

(defn init []
  (g :skip_ts_context_commentstring_module true))

(defn main []
  (when-let [context-comment (md.prequire :ts_context_commentstring)]
    (context-comment.setup))

  (when-let [cmmnt (md.prequire :Comment)]
    (cmmnt.setup configs)
    (let [api (md.prequire :Comment.api)]
      (nnoremap
        ; for some reason, vim doesn't recognize C-/ as a valid mapping
        ; but it does recognize C-_ so we use that instead
        :<C-_>
        (fn comment-section []
          (if (= vim.v.count 0)
            (api.toggle.linewise.current)
            (api.toggle.linewise.count vim.v.count))))

      (xnoremap :<C-_> "<Plug>(comment_toggle_linewise_visual)"))))
