(module plugins.lualine
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn file-status []
  (->
    ""
    (#(if (or (not (bo modifiable)) (bo readonly))
        (.. $ "%#BerksStatusLineInfo#%#BerksStatusLineInfoInverse#  %#BerksStatusLineInfo#")
        $))
    (#(if (bo modified)
        (.. $ "%#BerksStatusLineRed#%#BerksStatusLineRedInverse#  %#BerksStatusLineRed#")
        $))))

(defn navic-location []
  (if-let [navic (md.prequire :nvim-navic)]
    (navic.get_location)
    ""))

(def- config
  {:options
   {:icons_enabled true
    :theme :dracula

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
    [{1 :diagnostics
      :sources [:nvim_lsp :ale]
      :sections [:error :warn]
      :diagnostics_color
      {:error :BerksStatusLineRedInverse
       :warn :BerksStatusLineInfoInverse}
      :symbols {:error " " :warn " "}
      :separator {:left "" :right ""}}
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
