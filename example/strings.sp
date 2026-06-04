{ strings.sp -- demonstrates the str/ family. }
{                                              }
{ Run:  ./spci example/strings.sp              }

"github.com/cstml/spacelang/stdlib/str.sp" require

{ ----- C primitives ----- }

"--- str/cat: concat two strings ---" .
"foo" "bar" str/cat .              { → "foobar" }

"--- str/len: byte length ---" .
"hello" str/len .                  { → 5 }

"--- str/sub: bounds-clamped substring ---" .
"hello, world" 7 5 str/sub .       { → "world" }
"abc" 1 100 str/sub .              { → "bc"   (len clamped) }
"abc" -5 2 str/sub .               { → "ab"   (start clamped) }

"--- str/ord / str/chr: byte ↔ number ---" .
"A" str/ord .                      { → 65 }
65 str/chr .                       { → "A" }

"--- str/eq: string equality ---" .
"foo" "foo" str/eq .               { → t }
"foo" "bar" str/eq .               { → 0 }


{ ----- spacelang helpers, built on the primitives ----- }

"--- str/empty? ---" .
"" str/empty? .                    { → t }
"x" str/empty? .                   { → 0 }

"--- str/head / str/tail ---" .
"hello" str/head .                 { → "h" }
"hello" str/tail .                 { → "ello" }

"--- str/reverse ---" .
"spacelang" str/reverse .          { → "gnalecaps" }

"--- str/repeat ---" .
"ab" 4 str/repeat .                { → "abababab" }

"--- str/starts-with? / str/ends-with? ---" .
"hello.sp" ".sp" str/ends-with? .  { → t }
"hello.sp" "he"  str/starts-with? . { → t }
"hi" "hello" str/starts-with? .    { → 0  (prefix longer than s) }

"--- str/contains? / str/index ---" .
"the quick brown fox" "brown" str/contains? .  { → t }
"the quick brown fox" "brown" str/index .      { → 10 }
"the quick brown fox" "purple" str/index .     { → -1 }


{ ----- compose: take basename of a path, then drop a suffix ----- }

"--- compose: turn 'src/lib/parse.sp' into 'parse' ---" .

{ basename: strip everything up to and including the last '/'. }
{ Implemented as a loop: while the path contains '/', advance  }
{ past the first one.                                          }
[
  [ ]                              { else: no '/' left, done }
  [ { then: strip past the first '/' and recurse }
    _bn-p "/" str/index 1 + [_bn-start] @
    _bn-p str/len _bn-start - [_bn-rest] @
    _bn-p _bn-start _bn-rest str/sub [_bn-p] @
    basename-loop
  ]
  _bn-p "/" str/contains?
  if
] [basename-loop] @

[ { p -- basename }
  [_bn-p] @
  basename-loop
  _bn-p
] [basename] @

"src/lib/util/parse.sp" basename .       { → "parse.sp" }

{ drop-suffix: if s ends with suffix, return s without it.    }
[ { s suffix -- s' }
  [_ds-suffix] @
  [_ds-s] @
  [ _ds-s ]                                                             { else: not a match, return unchanged }
  [ _ds-s 0 _ds-s str/len _ds-suffix str/len - str/sub ]                 { then: strip the suffix }
  _ds-s _ds-suffix str/ends-with?
  if
] [drop-suffix] @

"src/lib/util/parse.sp" basename ".sp" drop-suffix .   { → "parse" }
