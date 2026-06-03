{ stdlib/stdlib.sp -- umbrella that pulls in the full spacelang stdlib. }
{                                                                          }
{ Use:  "stdlib/stdlib.sp" :require                                        }
{ Paths are sibling-relative; :require follows the source file dir.        }

"github.com/cstml/spacelang/stdlib/str.sp"        :require
"github.com/cstml/spacelang/stdlib/log.sp"        :require
"github.com/cstml/spacelang/stdlib/fset.sp"       :require
"github.com/cstml/spacelang/stdlib/fmap.sp"       :require
"github.com/cstml/spacelang/stdlib/git.sp"        :require
"github.com/cstml/spacelang/stdlib/with-spco.sp"  :require
"github.com/cstml/spacelang/stdlib/test.sp"       :require
