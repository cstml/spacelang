{ log/log.sp — logging library, built on /bin/echo via sh/!.            }
{                                                                          }
{ Words:                                                                   }
{   log/info   msg --      "[info] msg" to stderr                          }
{   log/warn   msg --      "[warn] msg" to stderr                          }
{   log/error  msg --      "[error] msg" to stderr                         }
{   log/debug  msg --      "[debug] msg" if SPACELANG_DEBUG is set         }
{   log/raw    msg --      msg to stderr (no prefix)                       }
{                                                                          }
{ All levels go to stderr so they stay out of the way of words that pipe   }
{ stdout via sh/>.                                                         }
{                                                                          }
{ Assumes str/str.sp is already loaded by the caller.                      }

{ log/_emit: prefix msg -- ; shell out `echo '<prefix><msg>' >&2`.
  Caller is responsible for keeping single-quotes out of the message. }
[ { prefix msg -- }
  [log/_e-m] @  [log/_e-p] @
  `echo '` log/_e-p str/cat log/_e-m str/cat `' >&2` str/cat sh/! drop
] [log/_emit] @

[ `[info]  ` swap log/_emit ]  [log/info]  @
[ `[warn]  ` swap log/_emit ]  [log/warn]  @
[ `[error] ` swap log/_emit ]  [log/error] @
[ `` swap log/_emit ]          [log/raw]   @

[ { msg -- ; only emits when $SPACELANG_DEBUG is non-empty }
  [log/_d-m] @
  `SPACELANG_DEBUG` :env str/len 0 =
  [ ]
  [ `[debug] ` log/_d-m log/_emit ]
  if
] [log/debug] @
