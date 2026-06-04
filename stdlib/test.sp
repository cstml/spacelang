{ lib/test.sp — minimal test library.                                      }
{                                                                            }
{ Tracks pass/fail counts as strings (using shell expr for arithmetic)       }
{ so they can be used directly with str/cat. Call test/reset before a        }
{ suite, test/summary at the end.                                            }
{                                                                            }
{ Words:                                                                     }
{   test/reset                             zero counters                     }
{   test/heading     "title"               print section heading             }
{   test/assert      bool "name"           increment pass or fail + print     }
{   test/eq          actual expected "nm"  assert actual = expected           }
{   test/neq         actual expected "nm"  assert actual != expected          }
{   test/true?       bool "name"           assert true                        }
{   test/false?      bool "name"           assert false                       }
{   test/num-eq      actual expected "nm"  assert two numbers are =           }
{   test/str-eq      actual expected "nm"  assert two strings are =           }
{   test/summary                            print X pass, Y fail              }
{                                                                            }
{ Uses log (built-in) for output. Requires str.sp for str/cat.              }
{ Counters use scratch bindings and are NOT reentrant.                        }

"github.com/cstml/spacelang/stdlib/str.sp" require


{ --- internal helpers --- }

{ test/_inc: increment a string counter using shell expr.
  Stack: counter-string -- counter-string+1 }
[ { s -- s+1 }
  [test/_inc-s] @
  test/_inc-s `expr ` swap str/cat ` + 1` str/cat sh/> str/strip-nl
] [test/_inc] @

{ test/_ok: increment pass counter }
[ test/_pass test/_inc [test/_pass] @ ] [test/_ok] @

{ test/_nok: increment fail counter }
[ test/_fail test/_inc [test/_fail] @ ] [test/_nok] @

{ test/_report: bool "label" -- . Increments pass/fail, prints result. }
[ { bool label -- }
  swap
  [test/_r-ok] @
  [test/_r-label] @
  [ { else: fail }
    test/_nok
    `  FAIL ` test/_r-label str/cat log
  ]
  [ { then: pass }
    test/_ok
    `  PASS ` test/_r-label str/cat log
  ]
  test/_r-ok
  if
] [test/_report] @

{ test/_not: bool -- !bool. }
[ [ true ] [ false ] rot if ] [test/_not] @


{ --- public words --- }

{ test/reset: zero pass/fail counters (stored as strings) }
[ `0` [test/_pass] @  `0` [test/_fail] @ ] [test/reset] @

{ test/heading: print a section heading }
[ { title -- }
  `` log
  `--- ` swap str/cat ` ---` str/cat log
] [test/heading] @

{ test/assert: bool "name" -- }
[ test/_report ] [test/assert] @

{ test/eq: actual expected "name" -- }
[ { actual expected name -- }
  [test/_eq-name] @
  [test/_eq-exp] @
  [test/_eq-act] @
  test/_eq-act test/_eq-exp =
  test/_eq-name test/_report
] [test/eq] @

{ test/neq: actual expected "name" -- }
[ { actual expected name -- }
  [test/_neq-name] @
  [test/_neq-exp] @
  [test/_neq-act] @
  test/_neq-act test/_neq-exp =
  test/_not
  test/_neq-name test/_report
] [test/neq] @

{ test/true?: bool "name" -- }
[ test/_report ] [test/true?] @

{ test/false?: bool "name" -- }
[ swap test/_not swap test/_report ] [test/false?] @

{ test/num-eq: actual expected "name" -- assert two numbers are = }
[ { actual expected name -- }
  [test/_ne-name] @
  [test/_ne-exp] @
  [test/_ne-act] @
  test/_ne-act test/_ne-exp =
  test/_ne-name test/_report
] [test/num-eq] @

{ test/str-eq: actual expected "name" -- assert two strings are = }
[ { actual expected name -- }
  [test/_se-name] @
  [test/_se-exp] @
  [test/_se-act] @
  test/_se-act test/_se-exp str/eq
  test/_se-name test/_report
] [test/str-eq] @

{ test/summary: print pass/fail totals.
  Counters are strings, so str/cat works directly. }
[ { -- }
  test/_pass [test/_s-p] @
  test/_fail [test/_s-f] @
  `` log
  `=========================` log
  test/_s-f `0` str/eq
  [ { else: failures exist }
    `  PASS: ` test/_s-p str/cat `  FAIL: ` str/cat
    test/_s-f str/cat log
  ]
  [ { then: no failures, check pass count }
    test/_s-p `0` str/eq test/_not
    [ { else: no tests run }
      `  (no tests run)` log
    ]
    [ { then: all passed }
      `  ALL PASSED (` test/_s-p str/cat `)` str/cat log
    ]
    rot if
  ]
  rot if
  `=========================` log
] [test/summary] @
