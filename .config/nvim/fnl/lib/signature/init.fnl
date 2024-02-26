(module lib.signature
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

(def- sign-group :LibSignature)

(defn add-sign [text line id])

(defn remove-sign [id])

(defn update-signage [])
