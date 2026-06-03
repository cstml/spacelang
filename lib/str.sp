{ lib/str.sp -- string library, built on the str/ C primitives. }
{                                                                    }
{ Provided by the runtime (in C):                                    }
{   str/cat   a b -- a++b                                            }
{   str/len   s -- n                                                 }
{   str/sub   s start len -- substring  (bounds-clamped)             }
{   str/ord   s -- byte-value (or -1 for empty)                      }
{   str/chr   n -- 1-char string                                     }
{   str/eq    a b -- bool                                            }
{                                                                    }
{ Built here, on top:                                                }
{   str/empty?       s -- bool                                       }
{   str/head         s -- first char                                 }
{   str/tail         s -- everything past the first char             }
{   str/reverse      s -- reversed                                   }
{   str/repeat       s n -- s repeated n times                       }
{   str/starts-with? s p -- bool                                     }
{   str/ends-with?   s p -- bool                                     }
{   str/contains?    s p -- bool                                     }
{   str/index        s p -- first index of p in s, or -1             }
{   str/strip-nl     s -- s'  (drop single trailing newline)         }
{                                                                    }
{ Implementation note: helpers use global scratch bindings prefixed  }
{ with the function name (e.g. str/_rep-acc). They are therefore     }
{ NOT reentrant -- don't call str/repeat from inside str/repeat etc. }

{ ----- trivial helpers ----- }

[ str/len 0 = ] [str/empty?] @
[ 0 1 str/sub ] [str/head] @
[ dup str/len 1 - 1 swap str/sub ] [str/tail] @


{ ----- str/reverse: pure-stack (acc src) loop ----- }

[
  dup str/empty? [str/_rev-empty?] @
  [ { else: src non-empty }
    dup 0 1 str/sub                  { acc src head }
    swap                             { acc head src }
    dup str/len 1 - 1 swap str/sub   { acc head tail }
    swap                             { acc tail head }
    rot                              { tail head acc }
    str/cat                          { tail (head++acc) }
    swap                             { (head++acc) tail }
    str/_rev-loop
  ]
  [ { then: src empty } drop ]
  str/_rev-empty?
  if
] [str/_rev-loop] @

[ "" swap str/_rev-loop ] [str/reverse] @


{ ----- str/repeat: scratch-binding accumulator loop ----- }

[
  [ { else: still going }
    str/_rep-acc str/_rep-s str/cat [str/_rep-acc] @
    str/_rep-n 1 - [str/_rep-n] @
    str/_rep-loop
  ]
  [ { then: done } ]
  str/_rep-n 0 <=
  if
] [str/_rep-loop] @

[ { s n -- repeated }
  [str/_rep-n] @
  [str/_rep-s] @
  "" [str/_rep-acc] @
  str/_rep-loop
  str/_rep-acc
] [str/repeat] @


{ ----- str/starts-with? : scratch bindings for clarity ----- }

[ { s p -- bool }
  [str/_sw-p] @
  [str/_sw-s] @
  str/_sw-p str/len [str/_sw-plen] @
  [ { else: enough chars in s }
    str/_sw-s 0 str/_sw-plen str/sub
    str/_sw-p str/eq
  ]
  [ { then: s too short } false ]
  str/_sw-s str/len str/_sw-plen <
  if
] [str/starts-with?] @


{ ----- str/ends-with? ----- }

[ { s p -- bool }
  [str/_ew-p] @
  [str/_ew-s] @
  str/_ew-p str/len [str/_ew-plen] @
  [ { else: enough chars in s }
    str/_ew-s
    str/_ew-s str/len str/_ew-plen -
    str/_ew-plen
    str/sub
    str/_ew-p str/eq
  ]
  [ { then: s too short } false ]
  str/_ew-s str/len str/_ew-plen <
  if
] [str/ends-with?] @


{ ----- str/contains?: scratch-binding search loop ----- }

[
  [ { else: window in bounds }
    [ { else: no match, advance i }
      str/_c-i 1 + [str/_c-i] @
      str/_c-loop
    ]
    [ { then: match }
      true [str/_c-result] @
    ]
    str/_c-s str/_c-i str/_c-p str/len str/sub
    str/_c-p str/eq
    if
  ]
  [ { then: out of bounds, done } ]
  str/_c-i str/_c-p str/len + str/_c-s str/len >
  if
] [str/_c-loop] @

[ { s p -- bool }
  [str/_c-p] @
  [str/_c-s] @
  0 [str/_c-i] @
  false [str/_c-result] @
  str/_c-loop
  str/_c-result
] [str/contains?] @


{ ----- str/index: like contains?, but records first index, else -1 ----- }

[
  [ { else: window in bounds }
    [ { else: no match, advance i }
      str/_idx-i 1 + [str/_idx-i] @
      str/_idx-loop
    ]
    [ { then: record index }
      str/_idx-i [str/_idx-result] @
    ]
    str/_idx-s str/_idx-i str/_idx-p str/len str/sub
    str/_idx-p str/eq
    if
  ]
  [ { then: out of bounds, done } ]
  str/_idx-i str/_idx-p str/len + str/_idx-s str/len >
  if
] [str/_idx-loop] @

[ { s p -- index-or-(-1) }
  [str/_idx-p] @
  [str/_idx-s] @
  0 [str/_idx-i] @
  -1 [str/_idx-result] @
  str/_idx-loop
  str/_idx-result
] [str/index] @


{ ----- str/strip-nl: drop single trailing newline. Safe on empty. ----- }

[ { s -- s' }
  [str/_sn-s] @
  [
    [ str/_sn-s ]
    [ str/_sn-s 0 str/_sn-s str/len 1 - str/sub ]
    str/_sn-s str/_sn-s str/len 1 - 1 str/sub `
` str/eq
    if
  ]
  [ str/_sn-s ]
  str/_sn-s str/empty?
  if
] [str/strip-nl] @
