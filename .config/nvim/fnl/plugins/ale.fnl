(module :plugins.ale
  {:require {a aniseed.core
             r r
             nvim aniseed.nvim}})


(defn main []
  (->>
    {
     :ale_sign_error "✗"
     :ale_sign_warning ""
     :ale_echo_msg_format "%linter%(%code%): %s"
     :ale_sign_column_always 1
     :ale_virtualtext_cursor 1
     :ale_virtualtext_prefix "//=> "
     :ale_virtualtext_delay 300
     :ale_linters {:javascript [:eslint]
                   :typescript [:tslint :tsserver]
                   :pug [:pug-lint]
                   :clojure [:clj-kondo :joker]
                   :yaml.ansible [:ansible-lint]}}
    (r.to-pairs)
    (r.forEach
      (fn [[key val]]
        (tset nvim.g key val)))))
