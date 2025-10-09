(module plugins.oil
  {autoload
   {a aniseed.core
    r r
    oil oil}
   require {}
   import-macros []
   require-macros [macros]})
   

(defn main []
  (nnoremap :gef (fn [] (oil.open)) {:silent true})
  (oil.setup {:use_default_keymaps fa
              :columns [:icon]
              :keymaps {:g? {1 :actions.show_help :mode :n}     

                        :<Space> {1 :actions.select :mode :n}
                        :<C-v> {1 :actions.select :opts {:vertical true}}
                        :<C-s> {1 :actions.select :opts {:horizontal true}}

                        :<C-c> {1 :actions.close :mode :n}
                        :<C-p> {1 :actions.preview :mode :n}

                        :_  {1 :actions.open_cwd :mode :n}
                        :-  {1 :actions.parent :mode :n}

                        :<Left> {1 :actions.parent :mode :n}
                        :<Right> {1 :actions.select :mode :n}

                        :gs  {1 :actions.change_sort :mode :n}
                        :g.  {1 :actions.toggle_hidden :mode :n}}}))
