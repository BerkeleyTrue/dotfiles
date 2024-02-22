; (module lib.job-test
;   {autoload
;    {{: job*} lib.job
;     {: async
;      : await} lib.async}})


; TODO: need to add plenary to runtime path
; (deftest job*
;   (do
;     (var called 0)
;     (let [afn (as.async
;                  (fn []
;                    (set called (+ called 1))
;                    (let [(ok res) (as.await (run* {:command "echo" :args ["hello"]}))]
;                      (set called (+ called 1))
;                      (t.ok? ok res "echo command failed")
;                      (t.= :hello (. res 1)))))]
;       (afn (fn [ok res]
;              (t.ok? ok (.. "async function failed" res)))))))
