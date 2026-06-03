{ spcd.sp -- spaceforth package manager (v0).                            }
{ Built with `spcc --as spcd spcd.sp -o spcd`.                            }
{                                                                          }
{ Verbs:                                                                   }
{   spcd add URL[@ref]   append a line to deps.sp, then fetch              }
{   spcd fetch           read deps.sp, clone deps, write lib/lock.sp       }
{   spcd update          re-resolve all, rewrite lock.sp                   }
{   spcd install URL     fetch + spcc-compile + install binary             }
{   spcd list            print lock.sp contents                            }
{   spcd clean           rm -rf lib/                                       }

`str/str.sp`   :require
`log/log.sp`   :require
`git/git.sp`   :require
`fset/fset.sp` :require
`fmap/fmap.sp` :require


{ ===================================================================
  Temp dir / state files
  =================================================================== }

`mktemp -d` sh/> str/strip-nl [spcd-tmp] @

[ spcd-tmp `/visited` str/cat ]  [tmp-vis]   @
[ spcd-tmp `/bn-set`  str/cat ]  [tmp-bnset] @
[ spcd-tmp `/bn-map`  str/cat ]  [tmp-bnmap] @
[ spcd-tmp `/todo`    str/cat ]  [tmp-todo]  @
[ spcd-tmp `/lockmap` str/cat ]  [tmp-lock]  @
[ spcd-tmp `/newlock` str/cat ]  [tmp-new]   @

[ `rm -rf ` spcd-tmp str/cat sh/! drop ] [spcd-cleanup] @


{ ===================================================================
  Mode-aware deps/* words

  While loading deps.sp (todo mode), each deps/* word appends a
  "url|kind|ref" line to tmp-todo.

  While loading lock.sp (lock mode), they all behave as deps/sha and
  append a "url sha" pair to tmp-lock via fmap/put.
  =================================================================== }

[ [ { url -- }
    `echo '` swap str/cat `|head|' >> ` str/cat tmp-todo str/cat sh/! drop
] ] [deps/_head-todo] @

[ [ { url ref -- }
    [dbt-r] @ [dbt-u] @
    `echo '` dbt-u str/cat `|branch|` str/cat dbt-r str/cat `' >> ` str/cat
    tmp-todo str/cat sh/! drop
] ] [deps/_branch-todo] @

[ [ { url ref -- }
    [dtt-r] @ [dtt-u] @
    `echo '` dtt-u str/cat `|tag|` str/cat dtt-r str/cat `' >> ` str/cat
    tmp-todo str/cat sh/! drop
] ] [deps/_tag-todo] @

[ [ { url ref -- }
    [dst-r] @ [dst-u] @
    `echo '` dst-u str/cat `|sha|` str/cat dst-r str/cat `' >> ` str/cat
    tmp-todo str/cat sh/! drop
] ] [deps/_sha-todo] @

[ [ { url sha -- }
    [dlm-s] @ [dlm-u] @
    dlm-u dlm-s tmp-lock fmap/put
] ] [deps/_sha-lock] @

[
  deps/_head-todo   [deps/head]   @
  deps/_branch-todo [deps/branch] @
  deps/_tag-todo    [deps/tag]    @
  deps/_sha-todo    [deps/sha]    @
] [spcd-set-todo] @

[
  deps/_sha-lock [deps/head]   @
  deps/_sha-lock [deps/branch] @
  deps/_sha-lock [deps/tag]    @
  deps/_sha-lock [deps/sha]    @
] [spcd-set-lock] @

spcd-set-todo


{ ===================================================================
  fetch-one: clone ONE dep into lib/<basename>.
  Stack: url kind ref --
  Lock SHA (if present) overrides the manifest ref.
  =================================================================== }

[ { url kind ref target basename -- ; perform the git clone }
  [fo/bn] @  [fo/t] @  [fo/r] @  [fo/k] @  [fo/u] @
  `spcd: cloning ` fo/u str/cat log/info
  `mkdir -p lib` sh/! drop
  fo/k `head` str/eq
  [ fo/u fo/t git/clone ]
  [
    fo/k `sha` str/eq
    [ fo/u fo/r fo/t git/clone-sha ]
    [ fo/u fo/r fo/t git/clone-branch ]
    if
  ]
  if
] [fetch-one/clone] @

[ { url -- ; resolve HEAD SHA and append url+sha to tmp-new }
  [fo/u] @
  fo/u git/url-basename [fo/bn] @
  `lib/` fo/bn str/cat git/rev-parse [fo/sha] @
  fo/u fo/sha tmp-new fmap/put
] [fetch-one/record] @

[ { url kind ref -- }
  [fo/r] @  [fo/k] @  [fo/u] @
  fo/u git/url-basename [fo/bn] @
  `lib/` fo/bn str/cat [fo/t] @

  { Lock SHA, if any, overrides manifest ref. }
  fo/u tmp-lock fmap/get [fo/ls] @
  fo/ls str/len 0 >
  [ fo/ls [fo/r] @  `sha` [fo/k] @ ]
  [ ]
  if

  { Three cases: dir missing → clone; sha-kind + matches → skip;
    otherwise rm + re-clone.                                          }
  `test -d ` fo/t str/cat sh/! 0 =
  [
    fo/k `sha` str/eq
    [
      fo/t fo/r git/sha-eq
      [
        `spcd: ` fo/bn str/cat ` up to date` str/cat log/info
        fo/u fetch-one/record
      ]
      [
        `rm -rf ` fo/t str/cat sh/! drop
        fo/u fo/k fo/r fo/t fo/bn fetch-one/clone
        fo/u fetch-one/record
      ]
      if
    ]
    [
      `rm -rf ` fo/t str/cat sh/! drop
      fo/u fo/k fo/r fo/t fo/bn fetch-one/clone
      fo/u fetch-one/record
    ]
    if
  ]
  [
    fo/u fo/k fo/r fo/t fo/bn fetch-one/clone
    fo/u fetch-one/record
  ]
  if
] [fetch-one] @


{ ===================================================================
  verb-clean
  =================================================================== }

[
  `rm -rf lib` sh/! drop
  `spcd: clean done` log/info
  spcd-cleanup
] [verb-clean] @


{ ===================================================================
  process-fetch-loop: drain tmp-todo, fetch each, then write lock.sp
  =================================================================== }

{ pop-line: read first line, delete it from file. path -- line }
[
  dup `head -n 1 ` swap str/cat sh/> str/strip-nl
  swap `sed -i 1d ` swap str/cat sh/! drop
] [pop-line] @

{ cut-field: extract Nth |-field from a line. line n -- field }
[ { line n -- field }
  [cf-n] @  [cf-l] @
  `echo '` cf-l str/cat `' | cut -d'|' -f` str/cat cf-n str/cat
  sh/> str/strip-nl
] [cut-field] @

[
  { cond is true while there is more work in tmp-todo. }
  `test -s ` tmp-todo str/cat sh/! 0 =
  [
    tmp-todo pop-line [pf-line] @
    pf-line `1` cut-field [pf-url]  @
    pf-line `2` cut-field [pf-kind] @
    pf-line `3` cut-field [pf-ref]  @

    pf-url tmp-vis fset/has?
    [ process-fetch-loop ]
    [
      pf-url tmp-vis fset/add

      pf-url git/url-basename [pf-bn] @
      `lib/` pf-bn str/cat [pf-t] @

      pf-bn tmp-bnset fset/has?
      [
        pf-bn tmp-bnmap fmap/get [pf-collide] @
        `spcd: basename '` pf-bn str/cat `' claimed by both '` str/cat
        pf-collide str/cat `' and '` str/cat pf-url str/cat `'` str/cat log/info
      ]
      [
        pf-bn pf-url tmp-bnmap fmap/put
        pf-bn tmp-bnset fset/add

        pf-url pf-kind pf-ref fetch-one

        `test -f ` pf-t str/cat `/deps.sp` str/cat sh/! 0 =
        [
          spcd-set-todo
          `cat ` pf-t str/cat `/deps.sp` str/cat sh/> eval
        ]
        [ ]
        if
      ]
      if

      process-fetch-loop
    ]
    if
  ]
  [
    `spcd: writing lib/lock.sp` log/info
    tmp-new fmap/sort-u
    `mkdir -p lib` sh/! drop
    `(echo '{ generated by spcd -- do not edit by hand }' && cat ` tmp-new str/cat
    `) > lib/lock.sp` str/cat sh/! drop
    `spcd: fetch done` log/info
    spcd-cleanup
  ]
  if
] [process-fetch-loop] @


{ ===================================================================
  reset-state: blank out all tmp files
  =================================================================== }

[
  `mkdir -p ` spcd-tmp str/cat sh/! drop
  tmp-vis   fset/new
  tmp-bnset fset/new
  tmp-bnmap fmap/new
  tmp-todo  fset/new
  tmp-lock  fmap/new
  tmp-new   fmap/new
] [spcd-reset] @


{ ===================================================================
  verb-fetch / verb-update
  =================================================================== }

[
  `spcd: fetch` log/info
  spcd-reset

  { Phase 1: lock.sp if present }
  `test -f lib/lock.sp` sh/! 0 =
  [
    spcd-set-lock
    `cat lib/lock.sp` sh/> eval
  ]
  [ ]
  if

  { Phase 2: deps.sp }
  `test -f deps.sp` sh/! 0 =
  [
    spcd-set-todo
    `cat deps.sp` sh/> eval
    process-fetch-loop
  ]
  [
    `spcd: no deps.sp -- nothing to fetch` log/info
    spcd-cleanup
  ]
  if
] [verb-fetch] @

[
  `spcd: update` log/info
  spcd-reset
  spcd-set-todo
  `test -f deps.sp` sh/! 0 =
  [
    `cat deps.sp` sh/> eval
    process-fetch-loop
  ]
  [ `spcd: no deps.sp` log/info spcd-cleanup ]
  if
] [verb-update] @


{ ===================================================================
  verb-list
  =================================================================== }

[
  `test -f lib/lock.sp` sh/! 0 =
  [ `grep -v '^{' lib/lock.sp | grep -v '^$' || true` sh/> . ]
  [ `spcd: no lockfile; run 'spcd fetch'` log/info ]
  if
  spcd-cleanup
] [verb-list] @


{ ===================================================================
  Ref heuristic: ref -- kind   (40-hex → sha; v-prefix → tag; else branch)
  =================================================================== }

{ parse-at: split "url@ref" — emits url and ref ("" if no @). }
[ { s -- url ref }
  dup `@` str/contains?
  [
    dup `echo '` swap str/cat `' | cut -d@ -f1` str/cat sh/> str/strip-nl swap
    `echo '` swap str/cat `' | cut -d@ -f2-` str/cat sh/> str/strip-nl
  ]
  [ `` ]
  if
] [parse-at] @


[ { ref -- kind }
  `awk 'BEGIN{ r="` swap str/cat
  `"; if (length(r)==40) print "sha"; else if (substr(r,1,1)=="v") print "tag"; else print "branch" }'` str/cat
  sh/> str/strip-nl
] [ref-kind] @


{ ===================================================================
  verb-install
  =================================================================== }

[
  1 :argv [inst-url] @
  inst-url str/len 0 =
  [ `spcd: install: missing URL` log/info spcd-cleanup ]
  [
    inst-url parse-at [inst-r] @ [inst-u] @

    inst-u git/url-basename [inst-bn] @

    { bindir: $SPACELANG_BIN or $HOME/.local/bin }
    `SPACELANG_BIN` :env [inst-bin] @
    inst-bin str/len 0 =
    [
      `HOME` :env str/len 0 =
      [ `spcd: cannot determine install dir; set SPACELANG_BIN` log/info spcd-cleanup ]
      [ `HOME` :env `/.local/bin` str/cat [inst-bin] @ ]
      if
    ]
    [ ]
    if
    `mkdir -p ` inst-bin str/cat sh/! drop

    `mktemp -d` sh/> str/strip-nl [inst-tmp] @
    `spcd: install: fetching ` inst-u str/cat log/info

    inst-r str/len 0 =
    [ inst-u inst-tmp git/clone ]
    [
      inst-r ref-kind [inst-k] @
      inst-k `sha` str/eq
      [ inst-u inst-r inst-tmp git/clone-sha ]
      [ inst-u inst-r inst-tmp git/clone-branch ]
      if
    ]
    if

    `test -f ` inst-tmp str/cat `/main.sp` str/cat sh/! 0 =
    [
      `spcd: install: compiling ` inst-bn str/cat log/info
      `spcc --as ` inst-bn str/cat ` ` str/cat inst-tmp str/cat `/main.sp -o ` str/cat
      inst-bin str/cat `/` str/cat inst-bn str/cat sh/! [inst-rc] @
      inst-rc 0 =
      [ `spcd: installed ` inst-bn str/cat ` -> ` str/cat inst-bin str/cat `/` str/cat inst-bn str/cat log/info ]
      [ `spcd: spcc failed` log/info ]
      if
    ]
    [ `spcd: no main.sp at top of repo '` inst-u str/cat `'` str/cat log/info ]
    if

    `rm -rf ` inst-tmp str/cat sh/! drop
    spcd-cleanup
  ]
  if
] [verb-install] @


{ ===================================================================
  verb-add
  =================================================================== }

[
  1 :argv [add--a] @
  add--a str/len 0 =
  [ `spcd: add: missing URL[@ref]` log/info spcd-cleanup ]
  [
    add--a parse-at [add--r] @ [add--u] @
    add--r str/len 0 =
    [ `"` add--u str/cat `" deps/head
` str/cat ]
    [
      add--r ref-kind [add--k] @
      `"` add--u str/cat `" "` str/cat add--r str/cat `" deps/` str/cat add--k str/cat `
` str/cat
    ]
    if
    `cat >> deps.sp` sh/| drop

    verb-fetch
  ]
  if
] [verb-add] @


{ ===================================================================
  Dispatch
  =================================================================== }

[
  0 :argv [verb] @
  verb `add`     str/eq [ verb-add ] [
    verb `fetch`  str/eq [ verb-fetch ] [
      verb `update` str/eq [ verb-update ] [
        verb `list`  str/eq [ verb-list ] [
          verb `install` str/eq [ verb-install ] [
            verb `clean` str/eq [ verb-clean ] [
              `spcd: unknown verb '` verb str/cat `'` str/cat log/info
              spcd-cleanup
            ]  if 
          ]  if 
        ]  if 
      ]  if 
    ]  if 
  ]  if 
] [dispatch] @


:argc 0 =
[ `Usage: spcd add|fetch|update|list|install|clean [args]` log/info spcd-cleanup ]
[ dispatch ]
 if 
