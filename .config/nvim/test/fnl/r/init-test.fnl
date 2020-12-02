(module r-test
  {:require {r r}})

(deftest curry
  (t.ok? r.curry "is curry exported")
  (t.ok? (= (type r.curry) "function") "is a function")
  (t.ok?
    (=
     (type (r.curry #(+ $1 $2)))
     "function")
    "returns a function")
  (let [add1 ((r.curry #(+ $1 $2)) 1)]
    (t.ok?
      (=
        (type add1)
        "function")
      "curries to a function")

    (t.ok?
      (not=
        (type (add1 1))
        "function")
      "concludes function call")

    (t.ok?
      (not=
        (type (add1 1))
        "function")
      "concludes function call a second time")))
