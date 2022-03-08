(module :plugins.ale
  {require
   {: r
    : utils
    hl utils.highlights
    md utils.module}
   require-macros [macros]})


(defn main []
  (hl.link! :ALEVirtualTextError :Error)
  (hl.link! :ALEVirtualTextWarning :Todo)
  (hl.link! :ALEWarningSign :Comment)
  (utils.set-nvim-g!
    {
     :ale_sign_error " "
     :ale_sign_warning "🌑"
     :ale_echo_msg_format "%linter%(%code%): %s"
     :ale_sign_column_always true
     :ale_virtualtext_cursor false
     :ale_virtualtext_prefix "//=> "
     :ale_virtualtext_delay 300
     :ale_linters
     {:haskell [:stack-build :hls]
      :javascript [:eslint]
      :typescript [:tslint :tsserver :eslint]
      :pug [:pug-lint]
      :clojure [:clj-kondo]
      :yaml.ansible [:ansible-lint]}})

  (when-let [ale (md.ppackadd :ale)]
    (->>
      {:<leader>ak "<Plug>(ale_previous_wrap)"
       :<leader>aj "<Plug>(ale_next_wrap)"}
      (r.to-pairs)
      (r.for-each (fn [[from to]] (utils.nmap from to))))))
