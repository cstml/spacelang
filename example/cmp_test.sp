{ cmp_test.sp — exercises the total ordering of < > <= >= and =.
  Rank order: number < bool/word < thunk < string. }

"github.com/cstml/spacelang/stdlib/str.sp"  require
"github.com/cstml/spacelang/stdlib/test.sp" require

test/reset

"numbers" test/heading
1 2 < "1<2"   test/true?
2 1 < "2<1"   test/false?
2 2 <= "2<=2" test/true?
3 2 >  "3>2"  test/true?
5 5 =  "5=5"  test/true?
5 6 =  "5=6"  test/false?

"strings (lex byte order)" test/heading
"a" "b" <    "a<b"      test/true?
"b" "a" <    "b<a"      test/false?
"abc" "abd" < "abc<abd" test/true?
"a" "a" =    "a=a"      test/true?
"a" "b" =    "a=b"      test/false?

"atoms (bool < bool by name)" test/heading
false true < "false<true (alphabetic)" test/true?
true true =  "true=true" test/true?

"cross-rank: number < atom < fun < bitstring" test/heading
0 false <    "0<false"   test/true?
true [x] <   "true<[x]"  test/true?
[x] "x" <    "[x]<x"     test/true?
1 "a" <      "1<a"       test/true?
"a" 1 >      "a>1"       test/true?

"= is structural; cross-type is nil" test/heading
1 "1" =                   "1 vs '1'"       test/false?
[1 2 +] [1 2 +] =         "[1 2 +]=[1 2 +]" test/true?
[1] [2] =                 "[1]=[2]"        test/false?

test/summary
