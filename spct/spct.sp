{ spct.sp — spacelang test runner, dogfooded.                              }
{ Compiled into the `spct` binary via:                                      }
{   spcc spct.sp -o spct                                                    }
{ Usage:                                                                    }
{   spct test1.sp test2.sp ...     run test files through the test library  }
{   spct                           print usage                              }
{                                                                            }
{ Loads test.sp stdlib and evals each file arg. Test files call             }
{ test/reset and test/summary themselves.                                    }

"github.com/cstml/spacelang/stdlib/str.sp"  :require
"github.com/cstml/spacelang/stdlib/test.sp" :require

{ --- load one test file via cat + eval (spcc can't inline runtime :require) --- }

[ { path -- }
  [spct/_load-path] @
  `cat ` spct/_load-path str/cat sh/> eval
] [spct/load-file] @

{ --- main --- }

[
  [ { else: no args, print usage }
    `Usage: spct test1.sp [test2.sp ...]` :log
  ]
  [ { then: load each file }
    0 [spct/_i] @
    [
      [ { else: more files }
        spct/_i :argv spct/load-file
        spct/_i 1 + [spct/_i] @
        spct/load-loop
      ]
      [ { then: done } ]
      spct/_i :argc >=
      if
    ] [spct/load-loop] @
    spct/load-loop
  ]
  :argc 0 >
  if
] [main] @

main
:bye
