(module slackline.mode-map
  {:require {:t :theme}})

(def m
  {:n
   {:name :NORMAL
    :fg (t.c.fg:light)
    :bg t.c.purple}
   :no
   {:name :OPERATOR
    :fg t.c.fg
    :bg t.c.purple}
   :i
   {:name :INSERT
    :fg t.c.bg
    :bg t.c.cyan}
   :v
   {:name :VISUAL
    :fg t.c.bg
    :bg t.c.yellow}
   :V
   {:name :VLINE
    :fg t.c.bg
    :bg (t.c.yellow:dark)}
   :\<C-V>
   {:name :VBLOCK
    :fg t.c.bgdark
    :bg (t.c.yellow:dark)}
   :multi
   {:name :MULTI
    :fg t.c.bgdark
    :bg (t.c.pink:dark)}
   :s
   {:name :SELECT
    :fg t.c.bg
    :bg t.c.yellow}
   :S
   {:name :SLINE
    :fg t.c.bgdark
    :bg (t.c.yellow:dark)}
   :c
   {:name :COMMAND
    :fg t.c.fg
    :bg (t.c.red:dark)}
   :R
   {:name :REPLACE
    :fg t.c.red
    :bg (t.c.bg:light)}
   :Rv
   {:name :REPLACE
    :fg t.c.red
    :bg (t.c.bg:light)}})
