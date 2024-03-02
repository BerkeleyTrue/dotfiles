(module scratch
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    as lib.async}
   require {}
   import-macros [[{: acase : alet} :lib.async-macros]]
   require-macros [macros]})
