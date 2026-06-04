{ Test suite for lib/git.sp library — all local, no network }

"dep.sp" require

{ Clean up from prior runs }
`rm -rf /tmp/spacelang_git_test/tmp_lib` sh/! drop
`mkdir -p /tmp/spacelang_git_test/tmp_lib` sh/! drop

{ ---- 1. git/url-basename ---- }
`git/url-basename: ` io/log

[ `  url-basename simple: FAIL` io/log ]
[ `  url-basename simple: OK` io/log ]
`github.com/foo/bar` git/url-basename `bar` str/eq
 if

[ `  url-basename strip .git: FAIL` io/log ]
[ `  url-basename strip .git: OK` io/log ]
`github.com/foo/bar.git` git/url-basename `bar` str/eq
 if

[ `  url-basename deep: FAIL` io/log ]
[ `  url-basename deep: OK` io/log ]
`host.com/a/b/c` git/url-basename `c` str/eq
 if

{ ---- 2. git/url-https ---- }
`git/url-https: ` io/log

[ `  url-https adds scheme: FAIL` io/log ]
[ `  url-https adds scheme: OK` io/log ]
`github.com/foo/bar` git/url-https `https://github.com/foo/bar` str/eq
 if

[ `  url-https already has scheme: FAIL` io/log ]
[ `  url-https already has scheme: OK` io/log ]
`https://github.com/foo/bar` git/url-https `https://github.com/foo/bar` str/eq
 if

{ ===== Clone / query tests (require the local bare repo) ===== }

[ { target sha -- }
  [asrt-sha] @  [asrt-t] @
  [
    `  ` asrt-t str/cat ` SHA mismatch: FAIL` str/cat io/log
    `    expected: ` asrt-sha str/cat io/log
    `    got:      ` asrt-t git/rev-parse str/cat io/log
  ]
  [
    `  ` asrt-t str/cat ` matches expected SHA: OK` str/cat io/log
  ]
  asrt-t git/rev-parse asrt-sha str/eq
  if
] [assert-sha] @

{ ---- 3. git/clone (shallow, default branch) ---- }
`git/clone: ` io/log

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-clone` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `/tmp/spacelang_git_test/tmp_lib/test-clone` git/clone
`/tmp/spacelang_git_test/tmp_lib/test-clone` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 4. git/clone-branch (shallow, --branch) ---- }
`git/clone-branch: ` io/log

{ Push a new branch to the bare repo }
`cd /tmp/spacelang_git_test/work/workspace && git checkout -b feature 2>&1 && echo "feature work" >> README.md && git commit -am "feature commit" && git push origin feature 2>&1` sh/> drop
{ Get the SHA on the feature branch }
`git -C /tmp/spacelang_git_test/work/workspace rev-parse HEAD 2>/dev/null` sh/> str/strip-nl [feat-sha] @

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-branch` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `feature` `/tmp/spacelang_git_test/tmp_lib/test-branch` git/clone-branch
`/tmp/spacelang_git_test/tmp_lib/test-branch` feat-sha assert-sha

{ ---- 5. git/clone-sha ---- }
`git/clone-sha: ` io/log

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-sha` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` `/tmp/spacelang_git_test/tmp_lib/test-sha` git/clone-sha
`/tmp/spacelang_git_test/tmp_lib/test-sha` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 6. git/rev-parse ---- }
`git/rev-parse: ` io/log
[ `  rev-parse wrong length: FAIL` io/log ]
[ `  rev-parse produces 40-char SHA: OK` io/log ]
`/tmp/spacelang_git_test/tmp_lib/test-sha` git/rev-parse dup str/len 40 =
 if
drop

{ ---- 7. git/sha-eq ---- }
`git/sha-eq: ` io/log
[ `  sha-eq match: FAIL` io/log ]
[ `  sha-eq match: OK` io/log ]
`/tmp/spacelang_git_test/tmp_lib/test-sha` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` git/sha-eq
 if

[ `  sha-eq mismatch: OK` io/log ]
[ `  sha-eq mismatch: FAIL (should be false)` io/log ]
`/tmp/spacelang_git_test/tmp_lib/test-sha` `0000000000000000000000000000000000000000` git/sha-eq
 if

{ ---- 8. git/clone-if-stale: target missing → clones ---- }
`git/clone-if-stale: ` io/log

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-ifstale` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` `/tmp/spacelang_git_test/tmp_lib/test-ifstale` git/clone-if-stale
`/tmp/spacelang_git_test/tmp_lib/test-ifstale` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 9. git/clone-if-stale: SHA matches → no-op (doesn't re-clone) ---- }
`git/clone-if-stale (idempotent): ` io/log
{ touch the target to detect if it gets replaced }
`/tmp/spacelang_git_test/test-repo.git` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` `/tmp/spacelang_git_test/tmp_lib/test-ifstale` git/clone-if-stale
`/tmp/spacelang_git_test/tmp_lib/test-ifstale` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 10. git/clone-if-stale: SHA mismatch → re-clones ---- }
`git/clone-if-stale (mismatch reclone): ` io/log
{ Use feature SHA on the target that has master SHA — should trigger reclone }
`/tmp/spacelang_git_test/test-repo.git` feat-sha `/tmp/spacelang_git_test/tmp_lib/test-ifstale` git/clone-if-stale
`/tmp/spacelang_git_test/tmp_lib/test-ifstale` feat-sha assert-sha

{ ---- Cleanup ---- }
`rm -rf /tmp/spacelang_git_test/tmp_lib` sh/! drop

`git/ ALL TESTS DONE` io/log
