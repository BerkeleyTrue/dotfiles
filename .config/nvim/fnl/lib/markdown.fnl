(module lib.markdown
  {autoload
   {a aniseed.core
    r r
    keys utils.keys}
   require {}
   import-macros []
   require-macros [macros]})

(defn switch-header-level [line]
  " # -> ## 
    ## -> ### 
    ### -> #### 
    #### -> ##### 
    ##### -> ###### 
    ###### -> #"
  (let [(indent hashes rest) (line:match "^(%s*)(#+)(.*)$")]
    (if hashes
      (let [new-level (-> (string.len hashes)
                          (% 6) 
                          (+ 1))]
        (.. indent (string.rep "#" new-level) rest))
      line)))
        

(comment 
  (switch-header-level "# Header 1")
  (switch-header-level "## Header 2")
  (switch-header-level "### Header 3")
  (switch-header-level "#### Header 4")
  (switch-header-level "##### Header 5")
  (switch-header-level "###### Header 6")
  (switch-header-level "No Header"))

(defn switch-checkbox-status [line]
  "- y     -> - [ ] y
   - [ ] y -> - [x] y
   - [x] y -> - y"
   (if 
     ; - [x] Task -> - Task
     (line:match "^%s*[*%-+] %[[xX]%]")
     (line:gsub "^(%s*[*%-+]) %[[xX]%]" "%1")

     ; - [ ] Task -> - [x] Task
     (line:match "^%s*[*%-+] %[ ?%]") 
     (line:gsub "^(%s*[*%-+]) %[ ?%]" "%1 [x]")

     ; - Task -> - [ ] Task
     (line:match "^%s*[*%-+] ") 
     (line:gsub "^(%s*[*%-+])" "%1 [ ]")

     :else line))

   
(comment
  (switch-checkbox-status "- Task")
  (switch-checkbox-status "- [ ] Task")
  (switch-checkbox-status "- [] Task")
  (switch-checkbox-status "- [x] Task"))

(defn swith-line-markdown []
  (let [line (n get-current-line)
        new-line (-> line
                     (switch-header-level)
                     (switch-checkbox-status))]
    (when (not= line new-line)
      (n set-current-line new-line))))

(defn is-list-item? [line]
  "Check if line is a list item"
  (or (line:match "^%s*[*%-+] ")
      (line:match "^%s*%d+%. ")))

(defn is-empty-list-item? [line]
  "Check if line is an empty list item (just the bullet/number with no content)"
  (or (line:match "^%s*[*%-+]%s*$")
      (line:match "^%s*%d+%.%s*$")))

(defn is-empty-quote? [line]
  "Check if line is an empty quote block (just > markers with no content)"
  (and (line:match ">")
       (line:match "^[%s>]*$")))

(defn indent-list-item [line]
  "Indent a list item by 2 spaces"
  (if (is-list-item? line)
    (.. "  " line)
    line))

(defn dedent-list-item [line]
  "Dedent a list item by 2 spaces"
  (if 
    (line:match "^  %s*[*%-+] ") (line:sub 3)
    (line:match "^  %s*%d+%. ") (line:sub 3)
    :else line))

(comment
  (indent-list-item "- ")
  (indent-list-item "- Task")
  (indent-list-item "  - Nested Task")
  (dedent-list-item "  - Nested Task")
  (dedent-list-item "- Task")
  (dedent-list-item "- ")
  (is-empty-list-item? "- ")
  (is-empty-list-item? "-")
  (is-empty-list-item? "- Task")
  (is-empty-list-item? "1. ")
  (is-empty-list-item? "1. Task")
  (is-empty-quote? "> ")
  (is-empty-quote? ">")
  (is-empty-quote? "> > ")
  (is-empty-quote? "> text"))

(defn indent-current-list-item []
  (let [line (n get-current-line)
        new-line (indent-list-item line)]
    (if (not= line new-line)
      (n set-current-line new-line))))

(defn dedent-current-list-item []
  (let [line (n get-current-line)
        new-line (dedent-list-item line)]
    (if (not= line new-line)
      (n set-current-line new-line))))

(defn indent-list-item-or-tab []
  (let [line (n get-current-line)]
    (if-not (is-list-item? line)
      (keys.feed "<Tab>")
      (let [[row col] (n win_get_cursor 0)]
        (n set-current-line (indent-list-item line))
        (n win_set_cursor 0 [row (+ col 2)])))))

(defn dedent-list-item-or-shift-tab []
  (let [line (n get-current-line)
        new-line (dedent-list-item line)]
    (if (= line new-line)
      (keys.feed "<S-Tab>")
      (let [[row col] (n win_get_cursor 0)]
        (n set-current-line new-line)
        (n win_set_cursor 0 [row (math.max 0 (- col 2))])))))

(defn clear-empty-line-or-enter []
  "In insert mode: clear empty list items/quotes, otherwise normal Enter"
  (let [line (n get-current-line)]
    (if (or (is-empty-list-item? line)
            (is-empty-quote? line))
      (n set-current-line "")
      (keys.feed "<CR>" true))))

(defn main []
  (augroup LibMarkdown
    {:event :FileType
     :pattern "markdown"
     :callback
     (r.void
       (fn setup-markdown []
         ; Enable automatic list/quote continuation on Enter
         (ol! comments "b:*,b:-,b:+,n:>")
         (ol! formatoptions "tron")
         (ol! formatlistpat "^\\s*\\d\\+\\.\\s\\+\\|^\\s*[+-\\*]\\s\\+")
         (nnoremap
           "<space>"
           #(swith-line-markdown)
           {:desc "Markdown: Switch header level or checkbox status"
            :silent true
            :buffer true})
         (nnoremap
           "<Tab>"
           #(indent-current-list-item)
           {:desc "Markdown: Indent list item"
            :silent true
            :buffer true})
         (nnoremap
           "<S-Tab>"
           #(dedent-current-list-item)
           {:desc "Markdown: Dedent list item"
            :silent true
            :buffer true})
         (inoremap
           "<Tab>"
           #(indent-list-item-or-tab)
           {:desc "Markdown: Indent list item or insert tab"
            :silent true
            :buffer true})
         (inoremap
           "<S-Tab>"
           #(dedent-list-item-or-shift-tab)
           {:desc "Markdown: Dedent list item or shift-tab"
            :silent true
            :buffer true})
        (inoremap
           "<CR>"
           #(clear-empty-line-or-enter)
           {:desc "Markdown: Clear empty list item/quote or normal enter"
            :silent true
            :buffer true})))}))

(comment "
 - Task
 # header
         ")
