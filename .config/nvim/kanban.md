## To Do

- tonksy fnl format
- builtin taskell in markdown files

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
  - [x] implement auto updated-at date
  - [x] add translate yaml to toml if found
  - [x] ensure space before ref links
  - [ ] add zettel command
    - [ ] creates a temp file in 00-zettle
    - [ ] on save, prompts for title
    - [ ] kebab-cases file name and inserts metadata

## Done

- [x] figure out nvim-cmp whitespace trigger
- [x] update packers to use make setup
- [x] move lazy/aniseed to nix
- [x] figure out why n/N (hlslens) adds newline if no match
