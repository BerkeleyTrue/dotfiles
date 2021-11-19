(module :plugins.ale
  {:require {: r
             : utils}})


(defn main []
  (utils.set-nvim-g!
    {
     :ale_sign_error "✗"
     :ale_sign_warning ""
     :ale_echo_msg_format "%linter%(%code%): %s"
     :ale_sign_column_always 1
     :ale_virtualtext_cursor 1
     :ale_virtualtext_prefix "//=> "
     :ale_virtualtext_delay 300
     :ale_linters {:haskell [:stack-build :hls]
                   :javascript [:eslint]
                   :typescript [:tslint :tsserver]
                   :pug [:pug-lint]
                   :clojure [:clj-kondo]
                   :yaml.ansible [:ansible-lint]}})
  (let [(ok res) (pcall utils.ex.packadd :ale)]
    (if ok
      (->>
        {:<leader>ak "<Plug>(ale_previous_wrap)"
         :<leader>aj "<Plug>(ale_next_wrap)"}
        (r.to-pairs)
        (r.for-each (fn [[from to]] (utils.nmap from to))))
      (print (.. "couldn't load ale: " res)))))
