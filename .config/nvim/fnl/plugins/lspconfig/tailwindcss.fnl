(module plugins.lspconfig.tailwindcss
  {require
   {a aniseed.core
    r r
    utils utils
    md utils.module}
   require-macros [macros]})

(defn get-config []
  (let [base-conf (md.prequire :lspconfig.server_configurations.tailwindcss)]
    (r.merge
      base-conf
      {:default_config
       (r.merge
         base-conf.default_config
         {:settings
          {:tailwindCSS
           (r.merge
             base-conf.default_config.settings.tailwindCSS
             {:emmetCompletions true
              :includeLanguages {:pug "html"}})}
          :filetypes
          [:aspnetcorerazor
           :astro
           :astro-markdown
           :blade
           :django-html
           :htmldjango
           :edge
           :eelixir ; vim ft
           :ejs
           :erb
           :eruby ; vim ft
           :gohtml
           :haml
           :handlebars
           :hbs
           :html
           ; HTML (Eex)
           ; HTML (EEx)
           :html-eex
           :heex
           :jade
           :pug
           :leaf
           :liquid
           :markdown
           :mdx
           :mustache
           :njk
           :nunjucks
           :php
           :razor
           :slim
           :twig
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
           :svelte]})})))
