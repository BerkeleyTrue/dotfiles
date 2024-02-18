(module lib.job
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    as lib.async}
   require
   {Job plenary.job} ; autoload has issues with metatables
   require-macros [macros]})
; a wrapper around plenary.job in async/await

(defn run [opts cb]
  "Run a job with the given options and callback. (non-blocking)
   Returns a unsubscribe function.
   Callback is called with values ok res,
   when ok res is stdout, otherwise stderr."
  (var shutdown? false)
  (assert (r.fn? cb) "callback must be a function")
  (let [j (doto (Job:new (r.merge opts
                                  {:on_exit
                                   (fn [j status]
                                     (when (not shutdown?)
                                       (if (= status 0)
                                         (cb true (j:result))
                                         (cb false (j:stderr_result)))))}))
            (: :start))]
    (r.void
      #(do
        (set shutdown? true)
        (j:shutdown)))))

(comment
  (run {:command "sleep" :args ["4s"]} (fn [ok res] (a.println :ok ok :res res)))
  (let [unsubscribe (run {:command "sleep" :args ["4s"]} (fn [ok res] (a.println :ok ok :res res)))]
    (unsubscribe)))

(defn run! [opts]
  "Run a job and block until done, returning results.
  Returns values ok res, when ok res is stdout, otherwise stderr."
  (let [j (doto (Job:new opts)
            (: :start)
            (: :sync))]
    (if (= j.code 0)
      (values true j._stdout_results)
      (values false j._stderr_results))))

(comment
  (run! {:command "sleep" :args ["4s"]}) ; blocks
  :)


(defn run* [opts]
  "Run a job with the given options in an async context."
  (as.thunkify run opts))

(comment
  (let [afn (as.async
             (fn []
               (let [(ok res) (as.await (run {:command "echo" :args ["hello"]}))]
                 (a.println :ok ok :res res))))]
    (afn))
  :)
