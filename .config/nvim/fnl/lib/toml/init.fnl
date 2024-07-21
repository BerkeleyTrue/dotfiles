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
  (a.println :->bounds cursor (r.size toml))
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
    (a.println :char char ":")
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
    (set cursor (+ cursor (or steps 1)))
    (a.println :step (or steps 1) :buffer buffer :cursor cursor))

  (fn bounds []
    (->bounds cursor toml))

  (fn skip-while [pred]
    (while (and (pred)
                (bounds))
      (step)))

  (fn skip-line [] 
    (a.println :skip-line)
    (skip-while #(not= (char) "\n")))

  (fn skip-whitespace [] 
    (a.println :skip-whitespace)
    (skip-while #(match-char? ws)))
  
  (fn err [_message] 
    (let [_message (->err cursor toml _message)]
      (a.println :err-message _message)
      (set ok? false)
      (set message _message)))

  (fn parse-key []
    (a.println :parse-key buffer)
    (if 
      (r.empty? buffer)
      (err "Empty table namespace")

      (. namespace buffer)
      (err "namespace" buffer " already occupied")

      (do (tset namespace buffer {})
          (set namespace (. namespace buffer))))
    (a.println :parse-key2 namespace))

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
    (a.println :parse-number)
    (var num "")
    (var date? false)

    (fn skip-char? []
      (or (match-char? ws)
          (match-char? nl)
          (is-char? :#)
          (is-char? ",")
          (is-char? "]")
          (is-char? "}")))

    (var done? false)
    (while (and ok?
                (not done?)
                (bounds))
      (if 
        (match-char? "[%+%-%.eE_0-9]")
        (err "Exponents unsupported")

        (skip-char?)
        nil
        
        (or (is-char? :T)
            (is-char? :Z)
            (is-char? "-"))
        (do 
          (set date? true)
          (set inner-done? false)
          (while (and ok? 
                      (not inner-done?)
                      (bounds))
            (if (skip-char?)
              (set inner-done? true)
              (set num (.. num (char))))))))
    (if date?
      {:value num :type :date}
      (let [float? (r.not-empty? (string.match num "%."))
            num (tonumber num)]
        {:type (if float? :float :int)
         :value num})))

  (fn parse-boolean []
    (a.println :parse-boolean (string.sub toml cursor (+ cursor 3)))
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
    (a.println :parse-val (or (char) :nil))

    (fn parse-array []
      (a.println :parse-array)
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

  (var loop 0)

  (while (and ok?
              (<= loop 10)
              (<= cursor (r.size toml)))
    (set loop (+ loop 1))
    (a.println :loop :ok ok? :char (char) :cursor cursor)
    (case (char)
      "#" ; skip comments
      (skip-line)

      "[" ; table key namespace
      (do
        (a.println :new-key)
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
                (do
                  ; (a.println :val val :key key :ns namespace)
                  (tset namespace key val.value)))
              (err (.. "Unable to parse value")))
            (err (.. "Expected key but found " key))))
        (set buffer "")
        (skip-whitespace)
        (when (is-char? "#") (skip-line))
        (when (and (not (match-char? nl)) ; this should be the end of the line
                   (bounds))
          (err (.. "Invalid char after val found: " (char))))))
    
    (set buffer (.. buffer (if (match-char? nl) "" (char))))
    (step))
  
  (values ok? (if ok? out message)))


(comment
  (logger.enable "lib.toml")
  (parse "foo = \"bar\"")
  (parse "foo = true")
  (parse "foo = false")
  (parse "foo = ['bar', 'baz']")
  (parse "foo = 'bar'\n[quz]\nx = 3\n"))
