(module lib.toml
  {autoload
   {a aniseed.core
    r r
    logger lib.log}
   require {}
   import-macros []
   require-macros [macros]})

(def log (logger.create :lib.toml))
(def ws "[\009\032]")
(def nl "[\10\13\10]")

(def escape 
  {:b "\b"
   :t "\t"
   :n "\n"
   :f "\f"
   :r "\r"
   "\"" "\""
   :\ :\ })

(defn ->char [cursor toml]
  (string.sub toml cursor cursor))

(defn ->bounds [cursor toml]
  "pred returns true if cursor is within bounds of toml"
  (<= cursor (r.size toml)))

(defn ->err [cursor toml message]
  (var c 0)
  (var line 1)
  (each [l (string.gmatch toml "(.-)\n") 
         &until (>= c cursor)]
    (set c (+ c (length l)))
    (set line (+ line 1)))
  (.. "TOML: " message " on line " line))

(comment
  (r.lmatch "(.-)\n" "foo\nbar\n")
  (->err 5 "foo\nbar\n baz\n" "bad bar")
  (->err 10 "foo\nbar\n baz\n" "bad bar"))

(defn find-next-line-cursor [cursor toml]
  (let [char (->char cursor toml)]    
    (if (= char "\n")                   
      cursor                             
      (let [next-cursor (+ cursor 1)]
        (if (->bounds next-cursor toml)
          next-cursor
          (find-next-line-cursor next-cursor toml))))))

(comment (find-next-line-cursor 3 "123456\njfjf"))

(defn parse [toml]
  (var cursor 1)
  (var buffer "")
  (var out {})
  (var ok? true)
  (var message "")
  (var namespace out) ; the current namespace to add keys

  (fn char [_n]
    (->char (+ cursor (or _n 0)) toml))

  (fn is-char? [ch _n]
    (= (char _n) ch))

  (fn match-char? [pattern _n]
    (->
      (char _n) 
      (string.match pattern) 
      (r.not-empty?))) 

  (fn step [steps]
    (set cursor (+ cursor (or steps 1))))

  (fn bounds []
    (->bounds cursor toml))

  (fn skip-while [pred]
    (while (and (pred)
                (bounds))
      (step)))

  (fn skip-line [] 
    (skip-while #(not= (char) "\n")))

  (fn skip-whitespace [] 
    (skip-while #(match-char? ws)))
  
  (fn err [_message] 
    (let [_message (->err cursor toml _message)]
      (set ok? false)
      (set message _message)))

  (fn parse-key []
    (if 
      (r.empty? buffer)
      (err "Empty table namespace")

      (. namespace buffer)
      (err "namespace" buffer " already occupied")

      (do (tset namespace buffer {})
          (set namespace (. namespace buffer)))))

  (fn parse-string []
    (var out "")
    (var end? false)
    (let [multiline? (= (char) (char 1) (char 2))
          quote* (char)]
      (step (if multiline? 3 1)) ; skip quote
      (while (and ok? 
                  (not end?)
                  (bounds))
        (if (is-char? quote*) ; check if closing string
          (do
            (step
              (if (and multiline?
                      (= (char) (char 1) (char 2)))
                3
                1))
            (set end? true))
          (if 
            (and multiline? 
                 (match-char? nl))
            (err "Sinle line string cannot contain line breaks")

            (if (= quote* (char))
              (if (and multiline? (match-char? nl 1))
                (do 
                  (step 1)
                  (var done? false)
                  (while (and (bounds)
                              (not done?))
                    (if (and (not (match-char? ws))
                             (not (match-char? nl)))
                      (set done? true)
                      (step))))
                (if 
                  (r.not-empty? (. escape (char 1)))
                  (do
                    (set out (.. out (char 1)))
                    (step 2))

                  (or (= (char 1) "u")
                      (= (char 1) "U"))
                  (err "Unicode escape not supported")))
              (do
                (set out (.. out (char)))
                (step))))))
      {:value out 
       :type :string}))

  (fn parse-number []
    (var buff "")
    (var fract "")
    (var fract? false)
    (var exp "")
    (var exp? false)
    (var date? false)
    (var done? false)
    (var neg? false)

    (while (and ok?
                (not done?)
                (bounds))
      (if 
        (is-char? "_") ; whitespace in number
        buff
        
        (or (match-char? nl)
            (match-char? ws))
        (set done? true)

        (and (is-char? ".")
             (not date?))
        (err "Fractions not implamented")

        (is-char? "-")
        (set date? true)
        
        (or (is-char? :e)
            (is-char? :E))
        (err "Exponents not implamented"))
        
      (if 
        fract?
        (set fract (.. fract (char)))

        (and (not date?) exp?)
        (set exp (.. exp (char)))
          
        (when (not done?)
          (set buff (.. buff (char)))))

      (when (not done?) 
        (step)))

    (if date?
      {:type :date 
       :value buff}
      
      (let [num (tonumber buff)
            exp (if exp? (tonumber exp) 0)
            res (* num (^ 10 exp))]
        (if fract?
          {:type :float
           :value res}
          {:type :int
           :value (math.floor res)}))))


  (fn parse-boolean []
    (if-let [val (if 
                   (= (string.sub toml cursor (+ cursor 3)) "true")
                   {:value true
                    :type :boolean}
                
                   (= (string.sub toml cursor (+ cursor 4)) "false")
                   {:value false
                    :type :boolean})]

    
      (do
        (skip-whitespace)
        (skip-line)
        val)
      (err "Invalid boolean")))

  (fn parse-val []

    (fn parse-array []
      (step)
      (skip-whitespace)
      (var out [])
      (var end-found? false)

      (while (and ok?
                  (not end-found?)
                  (bounds))
        (if 
          (is-char? "]")
          (do
            (step)
            (set end-found? true))

          (match-char? nl)
          (do
            (step)
            (skip-whitespace))
          
          (is-char? "#")
          (skip-line)

          (is-char? ",")
          (do
            (step)
            (skip-whitespace))

          (if-let [val (parse-val)]
            (do
              (table.insert out val.value)
              (skip-whitespace))
            (set end-found? true))))
      {:value out
       :type :array})


    (if 
      (or (is-char? "\"")
          (is-char? "'"))
      (parse-string)

      (match-char? "[%+%-0-9]")
      (parse-number)

      (is-char? "[")
      (parse-array)
      
      (match-char? "[tf]")
      (parse-boolean)
      
      (err (.. "Unexpected char found " (char)))))

  (while (and ok?
              (<= cursor (r.size toml)))
    (case (char)
      "#" ; skip comments
      (skip-line)

      "[" ; table key namespace
      (do
        (set buffer "") ; buffer for namespace
        (set namespace out) ; set up new namespace
        (var inner-done? false)
        (step)
        (while (and ok?
                    (not inner-done?)
                    (bounds))
          (case (char)
            "]" ; end of namespace
            (do (step)
                (parse-key true)
                (set buffer "")
                (set inner-done? true)
                (skip-whitespace))

            "." (err "implament namespaced keys")

            "\"" (err "implament quoted keys")

            "'" (err "implament quoted keys")

            _   
            (do
              (set buffer (.. buffer (char)))
              (step)))))
      

      "=" ; start of value
      (do 
        (step)
        (skip-whitespace)
        (let [key (r.trim buffer)] ; buffer should the key
          (if (r.not-empty? key)
            (if-let [val (parse-val)]
              (if (r.not-empty? (. namespace key))
                (err (.. "Expected namespace to be empty but found occupied for " key))
                (tset namespace key val.value))
              (err (.. "Unable to parse value")))
            (err (.. "Expected key but found " key))))
        (set buffer "")
        (skip-whitespace)
        (when (is-char? "#") 
          (skip-line))
        (when (and (not (match-char? nl)) ; this should be the end of the line
                   (bounds))
          (err (.. "Invalid char after val found: " (char))))))
    
    (set buffer (.. buffer (if (match-char? nl) "" (char))))
    (step))
  
  (values ok? (if ok? out message)))


(comment
  (logger.enable "lib.toml")
  (parse "foo = \"bar\" ")
  (parse "foo = true\n bar = \"baz\"")
  (parse "foo = false")
  (parse "foo = ['bar', 'baz']")
  (parse "foo = 'bar'\n[quz]\nx = 3\ndog = \"cat\"")
  (parse "foo = 2024-02-04")
  (parse "created-at = 2024-05-23\ntitle = \"credit\""))

(defn encode [tbl]
  (defn kv [out key val]
    (.. out key " = " val "\n"))

  (defn encode-val [val]
    (match (type val)
      :boolean (tostring val)
      :number (tostring val)

      :string 
      (let [date? (string.match val "%d%d%d%d%-%d%d%-%d%d")
            val (string.gsub val "\\" "\\\\")
            quote* (if (string.match val "\n") "\"\"\"" "\"")] ; TODO: add escape escapes
        (if date?
          val
          (.. quote* val quote*)))
      
      :table
      (if (> (table.maxn val) 0)
        (..
          "[\n  "
          (->>
            (r.map encode-val val)
            (r.join ",\n  "))
          "\n]")
        (encode val))))

  (->> (r.to-pairs tbl)
       (r.sort-by (fn [[k1 val1] [k2 val2]] 
                    (match [(type val1) (type val2)]
                      [:string :table] true
                      [:table :string] false

                      [:table :table]
                      (match [(> (table.maxn val1) 0) (> (table.maxn val2) 0) (r.empty? val1) (r.empty? val2)]
                        [_ _ true true] true ; both are empty and won't be encoded
                        ; both are non-empty and either seq table or kv table
                        ; use keys to sort
                        (where [t1len? t2len? false false] (r.xnor t1len? t2len?)) (< k1 k2) 
                        ; both are non-empty, but different types of tables
                        ; seq tables go first
                        [t1len? _ false false] t1len?) 

                      _ (< k1 k2))))
       (r.reduce 
         (fn [out [key val]]
           (let [val* (encode-val val)]
             (if (and (r.table? val) ; try to identify kv tables, only works on non-empty seq-tables
                      (= (table.maxn val) 0))
               (if (r.empty? val) ; only encode kv tables if they are not empty
                 out
                 (.. out "[" key "]\n" val* "\n"))
               (kv out key val*))))
         "")
       (r.trim)))

(comment
  (encode {:foo true})
  (encode {:foo "bar"})
  (encode {:foo "2024-04-04"})
  (encode {:bar [:foo :bar]})
  (encode {:bar [:foo 3 "2024-04-04"]})
  (encode {:bar {:foo :baz}})
  (encode {:bar {}})
  (encode {:a :a 
           :b {:z :y} 
           :f {:z :y} 
           :t "hello" 
           :d [:foo :bar :baz]
           :z [:foo :bar :baz]}))
