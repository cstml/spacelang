{ with-spco.sp -- caller helper library }
{ Pre-condition: an spco node is reachable on the bus, started as:    }
{   spco --bus /tmp/spacelang --serve                                  }
{                                                                     }
{ Provides spco-aware variants of $ / $! / $? -- each ensures the     }
{ peer is up (via a spawn-node EVAL to spco) before sending direct:   }
{                                                                     }
{   spco/$    msg [name] --                like $   but ensures peer  }
{   spco/$!   term [name] --               like $!  but ensures peer  }
{   spco/$?   term [name] timeout-ms --    like $?  but ensures peer  }
{                                                                     }
{ Example:  "with-spco.sp" :require    "hi" [B] spco/$                }

{ Internal: build spco source text from a name --  "name" spawn-node  }
[
  '"' swap cat '" spawn-node' cat
] [_spco-src] @

{ Internal: ask spco to ensure [name] is up, then briefly sleep.       }
{ Stack effect: [name] -- [name]  (consumes net 0; [name] preserved)  }
[
  dup name>str         { [name] "name" }
  _spco-src            { [name] '"name" spawn-node'  -- auto-evals _spco-src }
  "spco" $!            { [name] -- (EVAL sent)                         }
  500 :sleep
] [_spco-ensure] @

{ Public: spco-aware versions of $, $!, $?                            }
[ _spco-ensure  $  ] [spco/$]  @
[ _spco-ensure  $! ] [spco/$!] @
[ swap _spco-ensure swap  $? ] [spco/$?] @

{ Backward-compatible alias }
[ spco/$ ] [via-spco] @
