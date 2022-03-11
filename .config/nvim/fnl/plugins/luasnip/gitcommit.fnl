(module plugins.luasnip.gitcommit
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})
(def- docstring "
<type>(<scope>): <short summary>
  │       │             │
  │       │             └─⫸ Summary in present tense. Not capitalized. No period at the end.
  │       │
  │       └─⫸ Commit Scope: animations|bazel|benchpress|common|compiler|compiler-cli|core|
  │                          elements|forms|http|language-service|localize|platform-browser|
  │                          platform-browser-dynamic|platform-server|platform-webworker|
  │                          platform-webworker-dynamic|router|service-worker|upgrade|zone.js|
  │                          packaging|changelog|dev-infra|docs-infra|migrations|ngcc|ve
  │
  └─⫸ Commit Type: build|ci|docs|feat|fix|perf|refactor|style|test


summary:
 * use the imperative, present tense: "change" not "changed" nor "changes"
 * don't capitalize the first letter
 * no dot (.) at the end

Footer
BREAKING CHANGE: <breaking change summary>
<BLANK LINE>
<breaking change description + migration instructions>
<BLANK LINE>
<BLANK LINE>
Fixes #<issue number>")

(defn main [{: fmt : fmta : i : s : c : t}]
  {:commit
   (s
     {:trig "com"
      :name "commit"
      :dscr "Create a commit statement"
      : docstring}
     (fmta "
       <>(<>): <>

       <>"
       [(c 1
          [(t "feat")
           (t "fix")
           (t "chore")
           (t "refactor")
           (t "build")
           (t "ci")
           (t "docs")
           (t "perf")
           (t "test")
           (t "style")])
        (i 2 "subject")
        (i 3 "Summary in present tense. Not capitalized. No period at the end.")
        (i 0)]))

   :break
   (s
     {:trig "break"
      :name "breaking change"
      :docstr "Create a break change declaration"
      : docstring}
     (fmta "
       BREAKING CHANGE: <>"
       [(i 1 "Summary in present tense. Not capitalized. No period at the end.")]))

   :break
   (s
     {:trig "issue"
      :name "link issue"
      :docstr "link a github issue"
      : docstring}
     (fmta "
       Closes #<>"
       [(i 1 "1337")]))})
