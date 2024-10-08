(module plugins.lspconfig.tailwindcss
  {require
   {a aniseed.core
    r r
    utils utils
    md utils.module}
   require-macros [macros]})

(defn get-config []
  (let [base-conf (md.prequire :lspconfig.configs.tailwindcss)]
    {:settings
     {:tailwindCSS
      (r.merge
        base-conf.default_config.settings.tailwindCSS
        {:emmetCompletions false
         :classAttributes ["class" "className" "classList" "ngClass" "activeClass"]})}
     :init_options
     {:userLanguages
      {:pug "html"}}

     :autostart false
     :filetypes
     [
      :ejs
      :html
      :jade
      :pug
      :liquid
      :markdown
      :mdx
      :mustache
      :njk
      :nunjucks
      ; css
      :css
      :less
      :postcss
      :sass
      :scss
      :stylus
      :sugarss
      ; js
      :javascript
      :javascriptreact
      :reason
      :rescript
      :typescript
      :typescriptreact
      ; mixed
      :vue
      :svelte]}))
