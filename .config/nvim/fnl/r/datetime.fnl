(module r.datetime
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

(defn timestamp []
  "Get an ISO 8601 timestamp."
  (os.date "%Y-%m-%dT%H:%M:%S%z"))
