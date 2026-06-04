{ spct.sp — spacelang test runner, dogfooded.                              }
{ Compiled into the `spct` binary via:                                      }
{   spcc spct.sp -o spct                                                    }
{ Usage:                                                                    }
{   spct .                        find and run *_test.sp files recursively   }
{   spct dir/                     find and run *_test.sp files under dir/    }
{   spct file1.sp file2.sp ...    run specific files                        }
{   spct                          print usage                               }
{                                                                            }
{ Loads test.sp stdlib. Test files must be named *_test.sp and call         }
{ test/reset and test/summary themselves.                                    }

"github.com/cstml/spacelang/stdlib/str.sp"  require
"github.com/cstml/spacelang/stdlib/test.sp" require

{ --- helpers --- }

{ is-dir?: check if a path is a directory.  path -- bool }
[ { path -- bool }
  `test -d ` swap str/cat ` 2>/dev/null` str/cat sh/! 0 =
] [is-dir?] @

{ find-tests: discover *_test.sp files under a directory.
  Writes sorted list to /tmp/_spct_files.  dir -- count }
[ { dir -- count }
  `find ` swap str/cat ` -name '*_test.sp' | sort > /tmp/_spct_files` str/cat
  sh/! drop
  `wc -l < /tmp/_spct_files` sh/> eval
] [find-tests] @

{ pop-line: read first line from a file, delete it.  path -- line }
[ { path -- line }
  dup `head -n 1 ` swap str/cat sh/> str/strip-nl
  swap `sed -i 1d ` swap str/cat sh/! drop
] [pop-line] @

{ load-file: cat a .sp file and eval it.  path -- }
[ { path -- }
  [spct/_load-path] @
  `cat ` spct/_load-path str/cat sh/> eval
] [spct/load-file] @

{ --- load all test files from a directory --- }

[ { -- }
  [ { else: more lines to process }
    `/tmp/_spct_files` pop-line [spct/_f] @
    [ ]
    [ spct/_f spct/load-file
      spct/_n 1 + [spct/_n] @ ]
    spct/_f str/len 0 >
    if
    spct/_remaining 1 - [spct/_remaining] @
    spct/expand-loop
  ]
  [ { then: done } ]
  spct/_remaining 0 <=
  if
] [spct/expand-loop] @

[ { dir -- loaded-count }
  find-tests [spct/_remaining] @
  0 [spct/_n] @
  spct/expand-loop
  spct/_n
] [spct/load-dir] @

{ --- main --- }

[
  [ { else: no args, print usage }
    `Usage: spct [. | dir/ | file.sp ...]` io/log
    `  spct .            find and run *_test.sp files` io/log
    `  spct dir/         find and run *_test.sp under dir` io/log
    `  spct file.sp ...  run specific files` io/log
  ]
  [ { then: process each arg }
    0 [spct/_loaded] @
    0 [spct/_i] @
    [
      [ { else: more args }
        spct/_i io/argv [spct/_a] @
        [ { else: arg is a file: load directly }
          spct/_a spct/load-file
          spct/_loaded 1 + [spct/_loaded] @
        ]
        [ { then: arg is a directory: discover *_test.sp files }
          spct/_a spct/load-dir
          spct/_loaded + [spct/_loaded] @
        ]
        spct/_a is-dir?
        if
        spct/_i 1 + [spct/_i] @
        spct/arg-loop
      ]
      [ { then: all args processed }
        [ ]
        [ `spct: no *_test.sp files found` io/log ]
        spct/_loaded 0 =
        if
      ]
      spct/_i io/argc >=
      if
    ] [spct/arg-loop] @
    spct/arg-loop
  ]
  io/argc 0 >
  if
] [main] @

main
bye!
