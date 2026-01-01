(module lib.markdown
  {autoload
   {a aniseed.core
    r r}
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
    (if (not= line new-line)
      (n set-current-line new-line))))

(defn main []
  (augroup LibMarkdown
    {:event :FileType
     :pattern "markdown"
     :callback 
     (r.void
       (fn setup-markdown []
         (nnoremap 
           "<space>"
           #(swith-line-markdown)
           {:desc "Markdown: Switch header level or checkbox status"
            :silent true
            :buffer true})))}))
  
(comment "
 - Task
 # header
         ")
