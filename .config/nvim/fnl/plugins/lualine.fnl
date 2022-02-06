(module plugins.lualine
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

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
    [:filename]

    :lualine_x
    [:filetype]

    :lualine_y
    [:progress]

    :lualine_z
    [:location]}

   :inactive_sections
   {:lualine_a []
    :lualine_b []
    :lualine_c [:filename]
    :lualine_x [:location]
    :lualine_y []
    :lualine_z []}
   :tabline {}
   :extensions {}})

(defn main []
  (when-let [lualine (md.packadd-n-require :lualine.nvim :lualine)]
    (lualine.setup config)))
