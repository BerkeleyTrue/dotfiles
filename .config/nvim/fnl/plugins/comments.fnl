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
   {:basic true
    :extra true
    :extended false}
   :pre_hook pre-hook
   :post_hook nil})

(defn main []
  (when-let [cmmnt (md.packadd-n-require :comment.nvim :Comment)]
    (cmmnt.setup configs)))
