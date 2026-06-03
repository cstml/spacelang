{ spcd.sp -- spaceforth package manager (v0 MVP).                     }
{ Built with `spcc --as spcd spcd.sp -o spcd`.                         }
{                                                                     }
{ v0 scope (this file):                                                }
{   spcd add URL[@ref]   append a deps/* line to deps.sp, then fetch  }
{   spcd fetch           read deps.sp, git-clone each dep into lib/   }
{   spcd clean           rm -rf lib/                                  }
{                                                                     }
{ Punted to v1: lockfile, transitive deps, update, binary install,    }
{ kind heuristic, sort/dedupe.                                         }

"str/str.sp" :require


{ ===================================================================
  Helpers
  =================================================================== }

{ url-basename: last "/"-segment of a URL.                            }
[
  ub--p `/` str/contains?
  [ ]
  [
    ub--p `/` str/index 1 + [ub--s] @
    ub--p str/len ub--s - [ub--n] @
    ub--p ub--s ub--n str/sub [ub--p] @
    ub--loop
  ]
  rot if !
] [ub--loop] @

[ { url -- basename }
  [ub--p] @
  ub--loop
  ub--p
  { Strip a trailing ".git" if present so foo.git -> foo. }
  dup `.git` str/ends-with?
  [ ]                                       { false: no .git suffix -- noop }
  [ dup str/len 4 - 0 swap str/sub ]        { true: strip last 4 chars }
  rot if !
] [url-basename] @

{ url-as-git: prepend https:// only if url has no scheme yet. }
[ { url -- git-url }
  dup `://` str/contains?
  [ `https://` swap str/cat ]      { false: no scheme -- prepend }
  [ ]                              { true: already has scheme -- noop }
  rot if !
] [url-as-git] @


{ ===================================================================
  fetch-one: clone ONE dep into lib/<basename>.
  Stack: url kind ref --
  Idempotent: skip if lib/<basename> exists.
  =================================================================== }

[ { url kind ref -- }
  [fo--r] @
  [fo--k] @
  [fo--u] @
  fo--u url-basename [fo--bn] @
  `lib/` fo--bn str/cat [fo--t] @

  `test -d ` fo--t str/cat sh/! 0 =
  [
    { else: missing; clone. }
    `spcd: cloning ` fo--u str/cat :log
    `mkdir -p lib` sh/! drop

    fo--k `head` str/eq
    [
      fo--k `sha` str/eq
      [
        { branch or tag }
        `git clone --depth 1 --branch '` fo--r str/cat
        `' '` str/cat fo--u url-as-git str/cat `' '` str/cat
        fo--t str/cat `' >&2` str/cat sh/! drop
      ]
      [
        { sha }
        `git clone '` fo--u url-as-git str/cat `' '` str/cat
        fo--t str/cat `' >&2` str/cat sh/! drop
        `git -C '` fo--t str/cat `' checkout '` str/cat
        fo--r str/cat `' >&2` str/cat sh/! drop
      ]
      rot if !
    ]
    [
      { head }
      `git clone --depth 1 '` fo--u url-as-git str/cat `' '` str/cat
      fo--t str/cat `' >&2` str/cat sh/! drop
    ]
    rot if !
  ]
  [
    { then: already present, skip. }
    `spcd: ` fo--bn str/cat ` already present, skipping` str/cat :log
  ]
  rot if !
] [fetch-one] @


{ ===================================================================
  deps/* words
  =================================================================== }

[ { url -- }     `head` `` fetch-one ]                  [deps/head]   @
[ { url ref -- } [d-b-r] @ [d-b-u] @ d-b-u `branch` d-b-r fetch-one ] [deps/branch] @
[ { url ref -- } [d-t-r] @ [d-t-u] @ d-t-u `tag`    d-t-r fetch-one ] [deps/tag]    @
[ { url ref -- } [d-s-r] @ [d-s-u] @ d-s-u `sha`    d-s-r fetch-one ] [deps/sha]    @


{ ===================================================================
  Verbs
  =================================================================== }

[
  `rm -rf lib` sh/! drop
  `spcd: clean done` :log
] [verb-clean] @


[
  `test -f deps.sp` sh/! 0 =
  [ `spcd: no deps.sp -- nothing to fetch` :log ]
  [
    { Read & eval at runtime (sh/> + eval). We can't use :require here  }
    { because spcc would try to statically inline deps.sp at compile    }
    { time.                                                              }
    `cat deps.sp` sh/> eval
  ]
  rot if !
] [verb-fetch] @


[
  1 :argv [add--a] @
  add--a str/len 0 >
  [ `spcd: add: missing URL[@ref]` :log ]
  [
    add--a `@` str/contains?
    [
      { no ref }
      `"` add--a str/cat `" deps/head
` str/cat
      `cat >> deps.sp` sh/| drop
    ]
    [
      add--a `@` str/index [add--i] @
      add--a 0 add--i str/sub [add--u] @
      add--a add--i 1 + add--a str/len add--i - 1 - str/sub [add--r] @
      `"` add--u str/cat `" "` str/cat add--r str/cat `" deps/branch
` str/cat
      `cat >> deps.sp` sh/| drop
    ]
    rot if !
    verb-fetch
  ]
  rot if !
] [verb-add] @


{ ===================================================================
  Dispatch
  =================================================================== }

[
  0 :argv [verb] @
  verb `add`   str/eq [
    verb `fetch` str/eq [
      verb `clean` str/eq [
        `spcd: unknown verb '` verb str/cat `'` str/cat :log
      ] [ verb-clean ] rot if !
    ] [ verb-fetch ] rot if !
  ] [ verb-add ] rot if !
] [dispatch] @


:argc 0 =
[ dispatch ]
[ `Usage: spcd add|fetch|clean [args]` :log ]
rot if !
