{ lib/fmap.sp — file-backed string→string map.                          }
{                                                                          }
{ On-disk format: one "key value" pair per line. Keys may not contain     }
{ whitespace; values are everything after the first space.                }
{                                                                          }
{ Words:                                                                   }
{   fmap/new    path --              truncate (or create)                  }
{   fmap/put    key val path --      append a pair                         }
{   fmap/get    key path -- val      "" if absent                          }
{   fmap/has?   key path -- bool                                           }
{   fmap/sort-u path --              in-place sort -u                      }
{                                                                          }
{ Helpers use named scratch bindings; NOT reentrant.                       }

"github.com/cstml/spacelang/stdlib/str.sp" require

[ { path -- }
  `> ` swap str/cat sh/! drop
] [fmap/new] @


[ { key val path -- }
  [fm/p-p] @  [fm/p-v] @  [fm/p-k] @
  `echo '` fm/p-k str/cat ` ` str/cat fm/p-v str/cat `' >> ` str/cat
  fm/p-p str/cat sh/! drop
] [fmap/put] @


[ { key path -- val }
  [fm/g-p] @  [fm/g-k] @
  `grep -F '` fm/g-k str/cat ` ' ` str/cat fm/g-p str/cat
  ` 2>/dev/null | head -n 1 | cut -d' ' -f2-` str/cat
  sh/> str/strip-nl
] [fmap/get] @


[ { key path -- bool }
  [fm/h-p] @  [fm/h-k] @
  `grep -qF '` fm/h-k str/cat ` ' ` str/cat fm/h-p str/cat ` 2>/dev/null` str/cat
  sh/! 0 =
] [fmap/has?] @


[ { path -- }
  [fm/s-p] @
  `sort -u ` fm/s-p str/cat ` > ` str/cat fm/s-p str/cat `.tmp && mv ` str/cat
  fm/s-p str/cat `.tmp ` str/cat fm/s-p str/cat sh/! drop
] [fmap/sort-u] @
