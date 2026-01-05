(module lib.cursorline
  {autoload
   {a aniseed.core
    r r
    p theme.palette}
   require {}
   import-macros []
   require-macros [macros]})

           
; Cursor
; CursorLine
; CursorLineNr

(def conf
  {:n
   {:Cursor {:fg :fg :bg p.hex.text}
    :CursorLine {:fg p.none :bg p.hex.surface0}
    :CursorLineNr {:fg p.hex.lavender}}
   ; insert
   :i
   {:Cursor {:fg :fg :bg p.hex.sky}
    :CursorLine {:fg p.none :bg p.hex.surface1}
    :CursorLineNr {:fg p.hex.green :bg p.hex.surface1}}

   ; visual
   :v
   {:Cursor {:fg :fg :bg p.hex.peach}
    :CursorLineNr {:fg p.hex.peach}}

   ; select
   :s
   {:Cursor {:fg :fg :bg p.hex.peach}
    :CursorLineNr {:fg p.hex.peach}}

   ; change
   :c
   {:Cursor {:fg :fg :bg p.hex.flamingo}
    :CursorLine {:fg p.none :bg p.hex.surface1}
    :CursorLineNr {:fg p.hex.flamingo}}

   ; replace
   :r
   {:Cursor {:fg p.hex.mantle :bg p.hex.maroon}
    :CursorLine {:fg p.none :bg p.hex.surface1}
    :CursorLineNr {:fg p.hex.maroon}}

   ; operators
   :no {:c
        ; change
        {:Cursor {:fg :fg :bg p.hex.flamingo}
         :CursorLine {:fg p.none :bg p.hex.surface1}
         :CursorLineNr {:fg p.hex.flamingo}}
        ; yank
        :y
        {:Cursor {:fg :fg :bg p.hex.yellow}
         :CursorLine {:fg p.none :bg p.hex.surface1}
         :CursorLineNr {:fg p.hex.yellow}}
        ; delete
        :d
        {:Cursor {:fg :fg :bg p.hex.red}
         :CursorLine {:fg p.none :bg p.hex.surface1}
         :CursorLineNr {:fg p.hex.red}}
        ; change case
        :g
        {:Cursor {:fg :fg :bg p.hex.teal}
         :CursorLine {:fg p.none :bg p.hex.surface1}
         :CursorLineNr {:fg p.hex.teal}}}})



(defn main []
  (augroup
    :LibCursorLine
    {:event :ModeChanged
     :pattern "*:*"
     :callback 
     (r.void 
       (fn mode-changed []
         ; (print :mode-changed vim.v.event.new_mode)
         (let [mode vim.v.event.new_mode
               conf (if
                      (= :no mode)
                      nil ; 
                      ; no - operator mode
                      (r.starts-with? mode :no)
                      (let [op-conf (. conf :op)
                            op (v operator)]
                        (. op-conf op))

                      ; insert 
                      (r.some #(= mode $1) [:i :niI])
                      (. conf :i)

                      ; normal
                      (= mode :n)
                      (. conf :n)

                      ; change
                      (= mode :c)
                      (. conf :c)

                      ; visual
                      (r.some #(= mode $1) [:v :V "\x16"])
                      (. conf :v)
                      
                      ; selection
                      (r.some #(= mode $1) [:s :S "\x13"])
                      (. conf :s)
                      
                      ; selection 
                      (r.some #(= mode $1) [:R :niR :niV])
                      (. conf :r))

               conf (or conf {})
               cursor (. conf :Cursor)
               cursorline (. conf :CursorLine)
               cursorlinenr (. conf :CursorLineNr)]
               ; visual (. conf :Visual)]

           (when cursor (set-hl :Cursor cursor))
           (when cursorline (set-hl :CursorLine cursorline))
           (when cursorlinenr (set-hl :CursorLineNr cursorlinenr)))))}))
           ; (when visual (set-hl :Visual visual)))))}))
