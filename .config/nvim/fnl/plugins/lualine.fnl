(module plugins.lualine
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    p theme.palette}
   require-macros [macros]})

(defn file-status []
  (->
    ""
    (#(if (or (not (bo modifiable)) (bo readonly))
        (.. $ "%#BerksStatusLineInfo# %#BerksStatusLineInfoInverse# 󰌾 %#BerksStatusLineInfo#")
        $))
    (#(if (bo modified)
        (.. $ "%#BerksStatusLineMod# %#BerksStatusLineModInverse#  %#BerksStatusLineMod#")
        $))))

(defn navic-location []
  (if-let [navic (md.prequire :nvim-navic)]
    (navic.get_location)
    ""))

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
    [:mode]
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
