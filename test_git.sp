{ Test suite for git/git.sp library — all local, no network }

`str/str.sp` :require
`git/git.sp` :require

{ Clean up from prior runs }
`rm -rf /tmp/spacelang_git_test/tmp_lib` sh/! drop
`mkdir -p /tmp/spacelang_git_test/tmp_lib` sh/! drop

{ ---- 1. git/url-basename ---- }
`git/url-basename: ` :log

`github.com/foo/bar` git/url-basename `bar` str/eq
[ `  url-basename simple: OK` :log ]
[ `  url-basename simple: FAIL` :log ]
 if 

`github.com/foo/bar.git` git/url-basename `bar` str/eq
[ `  url-basename strip .git: OK` :log ]
[ `  url-basename strip .git: FAIL` :log ]
 if 

`host.com/a/b/c` git/url-basename `c` str/eq
[ `  url-basename deep: OK` :log ]
[ `  url-basename deep: FAIL` :log ]
 if 

{ ---- 2. git/url-https ---- }
`git/url-https: ` :log

`github.com/foo/bar` git/url-https `https://github.com/foo/bar` str/eq
[ `  url-https adds scheme: OK` :log ]
[ `  url-https adds scheme: FAIL` :log ]
 if 

`https://github.com/foo/bar` git/url-https `https://github.com/foo/bar` str/eq
[ `  url-https already has scheme: OK` :log ]
[ `  url-https already has scheme: FAIL` :log ]
 if 

{ ===== Clone / query tests (require the local bare repo) ===== }

[ { target sha -- }
  [asrt-sha] @  [asrt-t] @
  asrt-t git/rev-parse asrt-sha str/eq
  [
    `  ` asrt-t str/cat ` matches expected SHA: OK` str/cat :log
  ]
  [
    `  ` asrt-t str/cat ` SHA mismatch: FAIL` str/cat :log
    `    expected: ` asrt-sha str/cat :log
    `    got:      ` asrt-t git/rev-parse str/cat :log
  ]
  if
] [assert-sha] @

{ ---- 3. git/clone (shallow, default branch) ---- }
`git/clone: ` :log

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-clone` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `/tmp/spacelang_git_test/tmp_lib/test-clone` git/clone
`/tmp/spacelang_git_test/tmp_lib/test-clone` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 4. git/clone-branch (shallow, --branch) ---- }
`git/clone-branch: ` :log

{ Push a new branch to the bare repo }
`cd /tmp/spacelang_git_test/work/workspace && git checkout -b feature 2>&1 && echo "feature work" >> README.md && git commit -am "feature commit" && git push origin feature 2>&1` sh/> drop
{ Get the SHA on the feature branch }
`git -C /tmp/spacelang_git_test/work/workspace rev-parse HEAD 2>/dev/null` sh/> str/strip-nl [feat-sha] @

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-branch` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `feature` `/tmp/spacelang_git_test/tmp_lib/test-branch` git/clone-branch
`/tmp/spacelang_git_test/tmp_lib/test-branch` feat-sha assert-sha

{ ---- 5. git/clone-sha ---- }
`git/clone-sha: ` :log

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-sha` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` `/tmp/spacelang_git_test/tmp_lib/test-sha` git/clone-sha
`/tmp/spacelang_git_test/tmp_lib/test-sha` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 6. git/rev-parse ---- }
`git/rev-parse: ` :log
`/tmp/spacelang_git_test/tmp_lib/test-sha` git/rev-parse dup str/len 40 =
[ `  rev-parse produces 40-char SHA: OK` :log ]
[ `  rev-parse wrong length: FAIL` :log ]
 if 
drop

{ ---- 7. git/sha-eq ---- }
`git/sha-eq: ` :log
`/tmp/spacelang_git_test/tmp_lib/test-sha` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` git/sha-eq
[ `  sha-eq match: OK` :log ]
[ `  sha-eq match: FAIL` :log ]
 if 

`/tmp/spacelang_git_test/tmp_lib/test-sha` `0000000000000000000000000000000000000000` git/sha-eq
[ `  sha-eq mismatch: FAIL (should be false)` :log ]
[ `  sha-eq mismatch: OK` :log ]
 if 

{ ---- 8. git/clone-if-stale: target missing → clones ---- }
`git/clone-if-stale: ` :log

`rm -rf /tmp/spacelang_git_test/tmp_lib/test-ifstale` sh/! drop
`/tmp/spacelang_git_test/test-repo.git` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` `/tmp/spacelang_git_test/tmp_lib/test-ifstale` git/clone-if-stale
`/tmp/spacelang_git_test/tmp_lib/test-ifstale` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 9. git/clone-if-stale: SHA matches → no-op (doesn't re-clone) ---- }
`git/clone-if-stale (idempotent): ` :log
{ touch the target to detect if it gets replaced }
`/tmp/spacelang_git_test/test-repo.git` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` `/tmp/spacelang_git_test/tmp_lib/test-ifstale` git/clone-if-stale
`/tmp/spacelang_git_test/tmp_lib/test-ifstale` `0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a` assert-sha

{ ---- 10. git/clone-if-stale: SHA mismatch → re-clones ---- }
`git/clone-if-stale (mismatch reclone): ` :log
{ Use feature SHA on the target that has master SHA — should trigger reclone }
`/tmp/spacelang_git_test/test-repo.git` feat-sha `/tmp/spacelang_git_test/tmp_lib/test-ifstale` git/clone-if-stale
`/tmp/spacelang_git_test/tmp_lib/test-ifstale` feat-sha assert-sha

{ ---- Cleanup ---- }
`rm -rf /tmp/spacelang_git_test/tmp_lib` sh/! drop

`git/ ALL TESTS DONE` :log
