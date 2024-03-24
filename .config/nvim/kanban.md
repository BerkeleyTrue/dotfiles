# To Do

- builtin kanban in markdown files
  - use nvim ui to provide kanban boards
  - move tasks using vim motion
  - find kanbans within file
- defasync macro should fold through symbols and replace binds with await
  - modify acase to account for the above
- build vim-signature replacement
  > see: sign_place, sign_define
  > see: chentoast/marks.nvim
  - [x] bug: multiple marks do not show
  - [x] feat: refresh on a timer
  - [ ] bug: not clearing marks on move
  - [ ] bug: removes marks randomly?

# Doing
  
# Done (2024)

- [x] figure out nvim-cmp whitespace trigger
- [x] update packers to use make setup
- [x] move lazy/aniseed to nix
- [x] figure out why n/N (hlslens) adds newline if no match
- [x] integrate personal corpus
  - [x] feat: parsing frontmatter into datastructure
  - [x] feat: update frontmatter on save to toml
  - [x] feat: update ref defs on save
  - [x] feat: implement go to and shortcut create
    - [x] feat: create shortcut
      - [x] normal mode
      - [x] visual mode
    - [x] detect if already on shortcut
      - [x] pull label
      - [x] figure out filename
      - [x] glob for file
        - [x] edit to open/create file
  - [x] transpile lua code to fennel and commit
  - [x] rewrite
  - [x] implement auto updated-at date
  - [x] add translate yaml to toml if found
  - [x] ensure space before ref links
  - [x] add zettel command
    - [x] creates a temp file in 00-zettle
    - [x] on save, prompts for title
    - [x] kebab-cases file name and inserts metadata
  - [x] fix issue with opening files outside of corpus directory
    - use buffer directory instead of cwd
  - [x] insert mode '[' should create a completion list with corpus search
    - [x] list items from corpus
    - [x] add document view
    - add debounce
  - [x] ref def's should account for subfolders
    - shortcut should be file name
    - search for file name, add full path as ref def
  - [x] ensure whitespace between metadata and content
- [x] add async let for await syntax
- [x] add yadm-open-file-url and yadm-open-commit-url
  > yadm open was good, but commit was more complicated then I cared too
- [x] scroll-fix should run on window size change
- [x] update to supported fennel ts
- [x] feat(fnl): add folds to fennel files
- [x] feat(corpus): <C-]> should create ref links
- [x] bug(corpus): cmp execute ref def adds duplicates
- [x] scroll-fix should account for folds
- [x] create fennel help docs from fennel reference
  - ala luaref helpdocs
- [x] rewrite scroll fix
  - [x] original still buggy
  - [x] rethink algorithm 
- Corpus
  - [x] bug: C-] in ref def rewraps ref
  - [x] bug: glob doesn't account for kebab-case file names
