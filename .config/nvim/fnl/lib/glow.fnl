(module lib.glow
  {autoload
   {a aniseed.core
    r r
    {: run} lib.spawn}
   require {}
   import-macros []
   require-macros [macros]})

(defn main []
  (augroup :LibGlow
    {:event [:BufNewFile :BufRead]
     :pattern "*.md"
     :callback #(do 
                  (command! :Glow 
                    (fn display-glow-popup []
                      (let [file (vf expand "%:p")
                            cwd (vf fnamemodify file ":h")]
                        (run {:command :tmux 
                              :args [:display-pop :-T :Glow :-h :95% (.. "glow " file)]
                              :cwd cwd}
                          (fn [ok?] 
                            (when-not ok?
                              (n echo_err_writeln "Failed to display markdown"))))))
                    {:desc "Render markdown in a tmux popup"
                     :buffer 0}))}))
