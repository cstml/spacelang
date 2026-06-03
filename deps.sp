{ Module identity for this repo. The resolver reads deps/override lines  }
{ to rewrite logical URLs to local paths. `"."` here means: when a       }
{ require starts with "github.com/cstml/spacelang/", search from this    }
{ directory.                                                              }

"." "github.com/cstml/spacelang" deps/override
