(module lib.scp
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(def M {:state {}})
(def- scp_prefix "scp -o ConnectTimeout=5 ")

(defn edit [url]
  (let [(user rest) (url:match "^scp://(.*)[@](.*)$")
        user (or user :UNKNOWN)
        rest (or rest (url:match "^scp://(.*)$"))

        (path file extension) (rest:match "(.-)([^//]-([^//%.]+))$")

        tempdir (.. (vf tempname) :/scamp- user "@" path)

        tempfile (.. tempdir file)]


    (vf mkdir tempdir :p)
    (print (.. "Downloading " url " to " tempfile))
    (let [log (vf system (.. scp_prefix url " " tempfile))]

      (if (not= (v shell_error) 0) 
        (do
          (print (.. "Error: failed to edit " url))
          (print log)
          (command bdelete)
          true)
        (do
          (command edit tempfile)
          (bo! filetype extension)
          (tset M.state url tempfile)
          (tset M.state tempfile url)
          true)))))

(defn write [url]
  (command silent :write)
  (let [log (vf system (.. scp_prefix (vf expand "%") " " url))]
    (if (not= (v shell_error) 0) 
      (do
        (print (.. "Error: failed to write " url))
        (print log)
        false)
      true)))

(defn main []
  (augroup
    :Scp
    {:event [:BufReadCmd :FileReadCmd]
     :pattern :scp://*
     :callback (fn [args] (edit args.match))}

    {:event [:BufWriteCmd :FileWriteCmd]
     :pattern :/tmp/*/scamp-*
     :callback (fn [args] (write (. M.state args.match)))}

    {:event [:BufWriteCmd :FileWriteCmd]
     :pattern ["scp://*"]
     :callback (fn [args] (write args.match))}))
