{ stdlib/stdlib.sp -- umbrella that pulls in the full spacelang stdlib. }
{                                                                          }
{ Use:  "stdlib/stdlib.sp" :require                                        }
{ Paths are sibling-relative; :require follows the source file dir.        }

"str.sp"        :require
"log.sp"        :require
"fset.sp"       :require
"fmap.sp"       :require
"git.sp"        :require
"with-spco.sp"  :require
