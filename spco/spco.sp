{ spco.sp -- spacelang orchestrator, dogfooded }
{ Compiled into the `spco` binary via:                                }
{   spcc --as spco spco.sp -o spco                                    }
{ Then run as:                                                        }
{   spco --bus /tmp/spacelang --serve                                 }
{ Callers send EVAL ($!) messages containing                          }
{   "name" spawn-node     (auto-evals under current semantics)        }

{ spawn-node: pop name string, ensure an spci is running for it.     }
{ Idempotent via :exists. Uses :bus so the child binds the same bus. }
[
  dup [_n] @                  { stack ["X" "X"], @ pops [_n] then "X"; bind _n; stack ["X"] }
  :alive                      { pops "X", real connect-test; stack [cond] }
  [ "[spco] spawn " _n str/cat :log
    "spci --serve --name " _n str/cat
    " --bus " str/cat :bus str/cat
    " &" str/cat
    sh/! drop
    300 :sleep
  ]                            { el: ran when cond is nil (name missing) }
  [ ]                          { th: ran when cond is t (name exists)    }
  rot                          { stack [el th cond] for if               }
  if
] [spawn-node] @
