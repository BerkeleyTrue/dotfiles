(module plugins.corpus
  {:require {a aniseed.core}})

(global CorpusDirectories
  {"~/docs/corpus" {:autocommit true
                    :autoreference true
                    :autotitle true
                    :base "./corpus"
                    :transform "local"}})
