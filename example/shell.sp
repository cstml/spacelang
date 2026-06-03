{ shell.sp -- demonstrates the sh/ family of shell-out primitives. }
{                                                                  }
{ Run:  ./spci example/shell.sp                                    }

"../stdlib/str.sp" :require

{ ----- sh/! : run a command, push its exit status ----- }
{ Use when you care about success/failure and the side effect.    }

"echo --- sh/! prints to its own stdout ---" sh/! drop

"sh/! exit status was: " .
"true"  sh/! .          { → 0 }
"false" sh/! .          { → 256 (status << 8 from system()) }


{ ----- sh/> : run a command, push its captured stdout ----- }
{ Trailing \n stripped, matching shell $(...).               }

"sh/> captured: " .
"echo hello-from-capture" sh/> .         { → "hello-from-capture" }
"printf 'no-trailing-newline'" sh/> .    { → "no-trailing-newline" }


{ ----- composing sh/> with eval and str/ helpers ----- }

"eval'd captured number, plus one: " .
"echo 41" sh/> eval 1 + .                { → 42 }

"version string handled by str/ helpers:" .
"printf 'v1.2.3'" sh/>      { stack: "v1.2.3" }
str/tail                    { "1.2.3" }
.                           { → "1.2.3" }

"that version starts with '1.'?" .
"printf 'v1.2.3'" sh/> str/tail
"1." str/starts-with? .     { → t }


{ ----- pulling structured data out of the host ----- }

"my user name (captured): " .
"id -un" sh/> .             { → "<your username>" }


{ ----- sh/| : pipe stdin into a command, push its exit status ----- }

"grep finds 'needle' in piped stdin?" .
"haystack needle hay" "grep -q needle" sh/| .   { → 0 (found) }
"haystack hay"        "grep -q needle" sh/| .   { → 256 (not found, exit 1 << 8) }


{ ----- sh/|> : pipe stdin into a command, capture its stdout ----- }

"upper-case via tr:" .
"hello" "tr a-z A-Z" sh/|> .                    { → "HELLO" }

"word count of piped input:" .
"the quick brown fox" "wc -w" sh/|> .           { → "4" (wc pads with spaces) }


{ ----- chain sh/> into sh/|> for a full pipeline driven by spacelang ----- }

"sorted lines, all spacelang-driven:" .
"printf 'c\nb\na\n'" sh/>                       { capture: "c\nb\na" }
"sort" sh/|> .                                  { pipe to sort, capture: "a\nb\nc" }
