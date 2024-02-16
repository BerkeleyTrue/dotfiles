(module plugins.lualine
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    p theme.palette
    cl lib.color
    navic nvim-navic}
   require-macros [macros]})

(def- hx p.hex)

(defn init []
  (set-hl :BerksStatusLineMod          {:fg hx.yellow   :bg hx.base})
  (set-hl :BerksStatusLineModInverse   {:fg hx.base     :bg hx.yellow})
  (set-hl :BerksStatusLineInfo         {:fg hx.blue     :bg hx.base})
  (set-hl :BerksStatusLineInfoInverse  {:fg hx.base     :bg hx.blue})
  (set-hl :BerksStatusLineErr          {:fg hx.red      :bg hx.base})
  (set-hl :BerksStatusLineErrInverse   {:fg hx.base     :bg hx.red})
  (set-hl :BerksStatusLineMulti        {:fg hx.base     :bg hx.green :bold true})
  (set-hl :BerksStatusLineMultiInverse {:fg hx.green    :bg hx.base  :bold true}))

(defn file-status []
  (->
    ""
    (#(if (or (not (bo modifiable)) (bo readonly))
        (.. $ "%#BerksStatusLineInfo# %#BerksStatusLineInfoInverse# 󰌾 %#BerksStatusLineInfo#")
        $))
    (#(if (bo modified)
        (.. $ "%#BerksStatusLineMod# %#BerksStatusLineModInverse#  %#BerksStatusLineMod#")
        $))))

(defn navic-location [] (navic.get_location))

(defn format-mode [mode]
  (if (b visual_multi)
    (let [{: patterns} (vf VMInfos)
          pattern (or (. (or patterns []) 1) "")]
      (.. "%#BerksStatusLineMulti#  " mode " "
          "%#BerksStatusLineMultiInverse# 󱩾 \"" pattern "\""))
    mode))

(def- config
  {:options
   {:icons_enabled true
    :theme :catppuccin

    :component_separators
    {:left ""
     :right ""}

    :section_separators
    {:left ""
     :right ""}

    :disabled_filetypes {}
    :always_divide_middle true}

   :sections
   {:lualine_a
    [{1 :mode
      :fmt format-mode}]
    :lualine_b
    [:branch
     :diff
     :diagnostics]
    :lualine_c
    [navic-location]
    :lualine_x
    [{1 :lsp_progress
      :display_components [:lsp_client_name :spinner :percentage]
      :spinner_symbols ["🌑 "  "🌒 "  "🌓 "  "🌔 "  "🌕 "  "🌖 "  "🌗 "  "🌘 "]
      :timer {:progress_enddelay 500 :spinner 200 :lsp_client_name_enddelay 500}
      :colors {:percentage (p.get-color-by-name :text)
               :title (p.get-color-by-name :text)
               :message (p.get-color-by-name :text)
               :spinner (p.get-color-by-name :mauve)
               :lsp_client_name (p.get-color-by-name :sky)
               :use true}}
     {1 :diagnostics
      :sources [:nvim_lsp]
      :sections [:error :warn]
      :diagnostics_color
      {:error :BerksStatusLineErrInverse
       :warn :BerksStatusLineInfoInverse}
      :symbols {:error " " :warn " "}
      :separator {:left " " :right " "}}
     {1 :filetype
      :separator ""}]
    :lualine_y [:progress]
    :lualine_z [:location]}

   :inactive_sections
   {:lualine_a []
    :lualine_b []
    :lualine_c []

    :lualine_x [:location]
    :lualine_y []
    :lualine_z []}

   :tabline {}

   :winbar
   {:lualine_a
    [:mode]
    :lualine_b
    [:branch
     :diff
     :diagnostics]
    :lualine_c
    [{1 :filename
      :file_status false
      :path 1
      :separator false}
     file-status]}

   :inactive_winbar
   {:lualine_a []
    :lualine_b []
    :lualine_c
    [{1 :filename
      :file_status false
      :path 1
      :separator false}
     file-status]}
   :extensions {}})

(defn main []
  (when-let [lualine (md.prequire :lualine)]
    (lualine.setup config)))
