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
{ Example:  "with-spco.sp" require    "hi" [B] spco/$                }

{ Internal: name -- thunk that pushes the name string then calls      }
{ spawn-node. We build the source as text, then `eval` parses it into }
{ a thunk value (so the receiver gets a runnable thunk via EVAL).     }
[
  '[ "' swap str/cat '" spawn-node ]' str/cat eval
] [_spco-src] @

{ Internal: ask spco to ensure [name] is up, then briefly io/sleep.      }
{ Stack effect: [name] -- [name]  (consumes net 0; [name] preserved)  }
[
  dup wo/name>str         { [name] "name" }
  _spco-src            { [name] thunk }
  "spco" $!            { [name] -- (EVAL sent) }
  500 io/sleep
] [_spco-ensure] @

{ Public: spco-aware versions of $, $!, $?                            }
[ _spco-ensure  $  ] [spco/$]  @
[ _spco-ensure  $! ] [spco/$!] @
[ swap _spco-ensure swap  $? ] [spco/$?] @

{ Backward-compatible alias }
[ spco/$ ] [via-spco] @
