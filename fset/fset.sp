{ fset/fset.sp — file-backed set of strings (one element per line).      }
{                                                                          }
{ Words:                                                                   }
{   fset/new   path --              truncate (or create) path              }
{   fset/add   item path --         append item if not already present     }
{   fset/has?  item path -- bool    true if item ∈ set                    }
{                                                                          }
{ Like str/, helpers use named scratch bindings and are NOT reentrant.    }

{ Assumes str/str.sp is already loaded by the caller. }


[ { path -- }
  `> ` swap str/cat sh/! drop
] [fset/new] @


[ { item path -- bool }
  [fs/h-p] @  [fs/h-i] @
  `grep -qxF '` fs/h-i str/cat `' ` str/cat fs/h-p str/cat ` 2>/dev/null` str/cat
  sh/! 0 =
] [fset/has?] @


[ { item path -- }
  [fs/a-p] @  [fs/a-i] @
  [ `echo '` fs/a-i str/cat `' >> ` str/cat fs/a-p str/cat sh/! drop ]
  [ ]
  fs/a-i fs/a-p fset/has?
  if
] [fset/add] @
