(module r.init-test
  {:require {a aniseed.core
             r r}})

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

(deftest rearg
  (t.ok? r.rearg "is rearg exported")
  (t.ok? (= (type r.rearg) "function") "is a funtion")
  (let
    [func #(* (+ $1 $2) $3)
     refunc (r.rearg func [2 3 1])]

    (t.ok? (not (pcall r.rearg func [1 3 0])) "index should throw if 0 is in the index")
    (t.ok? (= (type refunc) "function") "(rearg func index) should return a function")
    (t.ok? (= (func 2 2 3) 12) "test my math")
    (t.ok? (= (refunc 2 2 3) 10) "(refunc a b c) should be call (func b c a)")))
