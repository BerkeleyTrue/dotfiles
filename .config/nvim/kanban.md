## To Do

- tonksy fnl format
- figure out why n/N (hlslens) adds newline if no match

## Doing

- [ ] integrate personal corpus
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
  - [ ] implement auto updated-at date
  - [ ] add translate yaml to toml if found
  - [ ] add zettel command

## Done

- figure out nvim-cmp whitespace trigger
- update packers to use make setup
- move lazy/aniseed to nix
