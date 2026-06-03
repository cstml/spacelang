{ lib/git.sp — git utility library for spaceforth.                        }
{                                                                          }
{ Words all prefixed git/ and use sh/! / sh/> / str/ under the hood.       }
{ Requires: lib/str.sp                                                     }
{                                                                          }
{ Operations:                                                              }
{   git/url-basename  url -- basename     last "/" segment, strip .git     }
{   git/url-https     url -- https-url    prepend https:// if no scheme    }
{   git/clone         url target --        shallow clone, default branch   }
{   git/clone-branch  url branch target -- shallow clone, given branch     }
{   git/clone-sha     url sha target --    full clone + checkout sha       }
{   git/rev-parse     target -- sha        get HEAD SHA of repo            }
{   git/sha-eq         target sha -- bool  true if HEAD == sha             }
{                                                                          }
{ Implementation note: helpers use global scratch bindings prefixed        }
{ with the function name (e.g. git/_ub-p). Like str/, they are NOT         }
{ reentrant — don't call git/ functions from within themselves.            }

{ Assumes lib/str.sp is already loaded by the caller. }


{ ===================================================================
  URL helpers
  =================================================================== }

{ git/url-basename: last "/"-segment of a URL, strip trailing .git. }
[
  [ ]
  [
    ub--p `/` str/index 1 + [ub--s] @
    ub--p str/len ub--s - [ub--n] @
    ub--p ub--s ub--n str/sub [ub--p] @
    ub--loop
  ]
  ub--p `/` str/contains?
  if
] [ub--loop] @

[ { url -- basename }
  [ub--p] @
  ub--loop
  ub--p [ub--q] @
  [ ub--q ]
  [ ub--q ub--q str/len 4 - 0 swap str/sub ]
  ub--q `.git` str/ends-with?
  if
] [git/url-basename] @

{ git/url-https: prepend https:// only if url has no scheme yet and isn't
  an absolute path (so local /tmp/... repos pass through unchanged). }
[ { url -- https-url }
  [uh--u] @
  [
    [ `https://` uh--u str/cat ]
    [ uh--u ]
    uh--u `/` str/starts-with?
    if
  ]
  [ uh--u ]
  uh--u `://` str/contains?
  if
] [git/url-https] @


{ ===================================================================
  Clone operations
  =================================================================== }

{ git/clone: shallow clone, default branch.  url target -- }
[ { url target -- }
  [gc-t] @  [gc-u] @
  `git clone --depth 1 ` gc-u git/url-https str/cat ` "` str/cat gc-t str/cat `" >&2` str/cat
  sh/! drop
] [git/clone] @

{ git/clone-branch: shallow clone with --branch.  url branch target -- }
[ { url branch target -- }
  [gcb-t] @  [gcb-b] @  [gcb-u] @
  `git clone --depth 1 --branch "` gcb-b str/cat `" ` str/cat
  gcb-u git/url-https str/cat ` "` str/cat gcb-t str/cat `" >&2` str/cat
  sh/! drop
] [git/clone-branch] @

{ git/clone-sha: full clone + checkout specific SHA.  url sha target -- }
[ { url sha target -- }
  [gcs-t] @  [gcs-s] @  [gcs-u] @
  { First: full clone }
  `git clone ` gcs-u git/url-https str/cat ` "` str/cat gcs-t str/cat `" >&2` str/cat
  sh/! drop
  { Then: checkout the SHA }
  `git -C "` gcs-t str/cat `" checkout "` str/cat gcs-s str/cat `" >&2` str/cat
  sh/! drop
] [git/clone-sha] @


{ ===================================================================
  Query operations
  =================================================================== }

{ git/rev-parse: get HEAD SHA of a repo.  target -- sha }
[ { target -- sha }
  `git -C "` swap str/cat `" rev-parse HEAD 2>/dev/null` str/cat
  sh/> str/strip-nl
] [git/rev-parse] @

{ git/sha-eq: check if repo HEAD matches a given SHA.  target sha -- bool }
[ { target sha -- bool }
  [gse-s] @  [gse-t] @
  gse-t git/rev-parse
  gse-s str/eq
] [git/sha-eq] @


{ ===================================================================
  Conditional clone (clone-if-stale): only clone if target is missing
  or HEAD doesn't match the expected SHA.
  url sha target -- (clones if needed)
  =================================================================== }

[ { url sha target -- }
  [gcis-t] @  [gcis-s] @  [gcis-u] @

  [
    { Dir missing: new clone }
    gcis-u gcis-s gcis-t git/clone-sha
  ]
  [
    { Dir exists: check if SHA matches }
    [
      { Mismatch: rm + re-clone }
      `rm -rf "` gcis-t str/cat `"` str/cat sh/! drop
      gcis-u gcis-s gcis-t git/clone-sha
    ]
    [ { Match: skip } ]
    gcis-t gcis-s git/sha-eq
    if
  ]
  `test -d "` gcis-t str/cat `"` str/cat sh/! 0 =
  if
] [git/clone-if-stale] @
