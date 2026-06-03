{ example/tests.sp — demo test suite for the test.sp stdlib.                }
{ Run via:  spct example/tests.sp     or     spci example/tests.sp          }

"../stdlib/str.sp"  :require
"../stdlib/test.sp" :require

test/reset

"core assertions" test/heading

1 2 + 3 "1+2=3"   test/eq
5 2 * 10 "5*2=10"  test/eq
3 4 * 13 "3*4!=13" test/neq

true  "true passes" test/assert
false "false fails" test/assert       { deliberate failure — exercises FAIL }

"booleans" test/heading

4 2 > "4>2" test/true?
0     "nil"  test/false?

"strings" test/heading

"hello" "world" str/cat
"hello" "world" str/cat "concat" test/str-eq

test/summary
