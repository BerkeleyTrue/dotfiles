(module lib.spawn
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    {: async
     : await
     : thunkify} lib.async}
   require {}
   require-macros [macros]})

;; see: https://github.com/Olical/conjure/blob/master/fnl/conjure/remote/stdio-rt.fnl

(def- uv vim.loop)

(defn run [opts cb]
  "Spawn a new process. Call a callback with ok and results, where results is
  stdout:string[] if ok and stderr:string if not."
  (var cancelled? false)
  (let [stdout (uv.new_pipe false)
        stderr (uv.new_pipe false)
        io {:handle nil
            :pid nil
            :output []
            :errput []}
        options (r.merge opts
                         {:stdio [nil stdout stderr]
                          :cwd (or opts.cwd ".")})
        wrapped (vim.schedule_wrap (fn [...]
                                     (when (not cancelled?)
                                       (cb ...))))]

    (fn destroy []
      ;; https://teukka.tech/vimloop.html
      (pcall #(stdout:read_stop))
      (pcall #(stderr:read_stop))
      (pcall #(stdout:close))
      (pcall #(stderr:close))
      (when io.handle
        ;; On macos, when handle:close is called, it leaves a zombie process
        ;; behind. Just calling process_kill does the job because the handle
        ;; will be closed automatically.
        (pcall #(uv.process_kill io.handle uv.constants.SIGINT)))
      nil)

    (fn on-exit [code signal]
      (wrapped
        (= code 0)
        (if (= code 0)
          io.output
          (r.join "\\n" io.errput)))
      (destroy))


    (fn on-message [source err chunk]
      (if err
        (do
          (wrapped false err)
          (destroy))
        (when chunk
          (->> chunk
              (r.split "\n")
              (r.reject r.empty?)
              (r.for-each
                (fn [chunk]
                  (table.insert source chunk)))))))

    (let [(handle pid-or-err) (uv.spawn opts.command options (vim.schedule_wrap on-exit))]
      (if handle
        (do
          (stdout:read_start (partial (vim.schedule_wrap on-message) io.output))
          (stderr:read_start (partial (vim.schedule_wrap on-message) io.errput))
          (tset io :handle handle)
          (tset io :pid pid-or-err))
        (do
          (wrapped false pid-or-err)
          (destroy))))

    (r.void
      #(do
         (set cancelled? true)
         (destroy)))))

(defn run* [opts]
  "Spawn a new process in an async context. Yields ok and results, where results is
  stdout:string[] if ok and stderr:string if not."
  (thunkify run opts))

(comment
  (let [unsubscribe (run {:command "sleep" :args ["5s"]} (fn [ok res] (a.println :ok ok :res res)))]
    (unsubscribe))
  (run {:command "ls" :args [:-a] :cwd (vf expand "%:h")}
       (fn [ok res]
           (assert ok res)
           (a.println res))))
