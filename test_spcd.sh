#!/usr/bin/env bash
# Integration tests for spcd. Each test runs in a fresh sandbox under
# $SANDBOX. Tests use local bare git repos in $FIXDIR — no network.
#
# Usage: ./test_spcd.sh

set -u

ROOT=$(cd "$(dirname "$0")" && pwd)
SPCD=$ROOT/spcd
SPCC=$ROOT/spcc
FIXDIR=/tmp/spacelang_git_test
SANDBOX=/tmp/spcd-test
PASS=0
FAIL=0
FAILED_NAMES=()

red()   { printf '\033[31m%s\033[0m' "$*"; }
green() { printf '\033[32m%s\033[0m' "$*"; }

assert() {
  local name=$1; shift
  if "$@" > /dev/null 2>&1; then
    echo "  $(green PASS) $name"; PASS=$((PASS+1))
  else
    echo "  $(red FAIL) $name -- $*"; FAIL=$((FAIL+1)); FAILED_NAMES+=("$name")
  fi
}

assert_contains() {
  local name=$1 file=$2 needle=$3
  if grep -qF "$needle" "$file" 2>/dev/null; then
    echo "  $(green PASS) $name"; PASS=$((PASS+1))
  else
    echo "  $(red FAIL) $name -- '$needle' not in $file"
    [ -f "$file" ] && sed 's/^/      /' "$file"
    FAIL=$((FAIL+1)); FAILED_NAMES+=("$name")
  fi
}

assert_missing() {
  local name=$1 path=$2
  if [ ! -e "$path" ]; then
    echo "  $(green PASS) $name"; PASS=$((PASS+1))
  else
    echo "  $(red FAIL) $name -- $path still exists"
    FAIL=$((FAIL+1)); FAILED_NAMES+=("$name")
  fi
}

# --------------------------------------------------------------------
# Fixture setup. Idempotent: rebuilt only on demand.
# --------------------------------------------------------------------
setup_fixtures() {
  mkdir -p "$FIXDIR"

  # test-repo.git is set up by test_git.sp; reuse it.
  if [ ! -d "$FIXDIR/test-repo.git" ]; then
    echo "Missing $FIXDIR/test-repo.git — run test_git.sp first to bootstrap."
    exit 1
  fi

  # dep-a.git: a single .sp file, no further deps.
  if [ ! -d "$FIXDIR/dep-a.git" ]; then
    rm -rf "$FIXDIR/_seed-a"
    mkdir -p "$FIXDIR/_seed-a"
    (
      cd "$FIXDIR/_seed-a"
      git init -q -b master
      echo '{ dep-a stub }' > lib.sp
      git add lib.sp
      git -c user.email=t@t -c user.name=t commit -q -m 'dep-a'
      git clone -q --bare . "$FIXDIR/dep-a.git"
    )
    rm -rf "$FIXDIR/_seed-a"
  fi

  # dep-b.git: declares dep-a as a transitive dep.
  if [ ! -d "$FIXDIR/dep-b.git" ]; then
    rm -rf "$FIXDIR/_seed-b"
    mkdir -p "$FIXDIR/_seed-b"
    (
      cd "$FIXDIR/_seed-b"
      git init -q -b master
      echo '{ dep-b stub }' > lib.sp
      cat > deps.sp <<EOF
"$FIXDIR/dep-a.git" deps/head
EOF
      git add lib.sp deps.sp
      git -c user.email=t@t -c user.name=t commit -q -m 'dep-b'
      git clone -q --bare . "$FIXDIR/dep-b.git"
    )
    rm -rf "$FIXDIR/_seed-b"
  fi

  # binrepo.git: a main.sp suitable for `spcd install`.
  if [ ! -d "$FIXDIR/binrepo.git" ]; then
    rm -rf "$FIXDIR/_seed-bin"
    mkdir -p "$FIXDIR/_seed-bin"
    (
      cd "$FIXDIR/_seed-bin"
      git init -q -b master
      echo '`hello from binrepo` :log' > main.sp
      git add main.sp
      git -c user.email=t@t -c user.name=t commit -q -m 'binrepo'
      git clone -q --bare . "$FIXDIR/binrepo.git"
    )
    rm -rf "$FIXDIR/_seed-bin"
  fi
}

# Per-test sandbox helpers.
fresh_sandbox() { rm -rf "$SANDBOX"; mkdir -p "$SANDBOX"; cd "$SANDBOX"; }

# --------------------------------------------------------------------
# Tests
# --------------------------------------------------------------------

# T1: `spcd add URL` — head ref, clones, writes lock
test_add_head() {
  echo "T1: spcd add URL (head)"
  fresh_sandbox
  "$SPCD" add "$FIXDIR/test-repo.git" > /dev/null 2>&1
  assert "deps.sp exists"     test -f deps.sp
  assert_contains "deps.sp records deps/head" deps.sp 'deps/head'
  assert "lib/test-repo cloned" test -d lib/test-repo
  assert "lock.sp exists"     test -f lib/lock.sp
  assert_contains "lock has master SHA" lib/lock.sp '0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a'
}

# T2: `spcd list` prints lock contents
test_list() {
  echo "T2: spcd list"
  # Reuse T1's sandbox.
  local out
  out=$("$SPCD" list 2>&1)
  if echo "$out" | grep -qF '0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a'; then
    echo "  $(green PASS) list prints SHA"; PASS=$((PASS+1))
  else
    echo "  $(red FAIL) list output:"
    echo "$out" | sed 's/^/      /'
    FAIL=$((FAIL+1)); FAILED_NAMES+=("T2:list-sha")
  fi
}

# T3: `spcd fetch` is idempotent
test_fetch_idempotent() {
  echo "T3: spcd fetch idempotent"
  local before after
  before=$(cat lib/lock.sp)
  "$SPCD" fetch > /dev/null 2>&1
  after=$(cat lib/lock.sp)
  if [ "$before" = "$after" ]; then
    echo "  $(green PASS) lock.sp unchanged"; PASS=$((PASS+1))
  else
    echo "  $(red FAIL) lock.sp changed across re-fetch"; FAIL=$((FAIL+1)); FAILED_NAMES+=("T3")
  fi
}

# T4: `spcd clean` removes lib/
test_clean() {
  echo "T4: spcd clean"
  "$SPCD" clean > /dev/null 2>&1
  assert_missing "lib/ removed" lib
}

# T5: `spcd add URL@<branch>` heuristic — non-hex, non-v → deps/branch
test_add_branch() {
  echo "T5: spcd add URL@feature (branch heuristic)"
  fresh_sandbox
  "$SPCD" add "$FIXDIR/test-repo.git@feature" > /dev/null 2>&1
  assert_contains "deps/branch recorded" deps.sp 'deps/branch'
  assert_contains "lock has feature SHA" lib/lock.sp '54c049cc2f66285604f32e8f75ec744f777465b0'
}

# T6: `spcd add URL@<40hex>` heuristic → deps/sha
test_add_sha() {
  echo "T6: spcd add URL@<sha> (sha heuristic)"
  fresh_sandbox
  "$SPCD" add "$FIXDIR/test-repo.git@0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a" > /dev/null 2>&1
  assert_contains "deps/sha recorded" deps.sp 'deps/sha'
  assert_contains "lock has pinned SHA" lib/lock.sp '0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a'
}

# T7: transitive deps — dep-b declares dep-a; both should land in lib/
test_transitive() {
  echo "T7: transitive deps via nested deps.sp"
  fresh_sandbox
  "$SPCD" add "$FIXDIR/dep-b.git" > /dev/null 2>&1
  assert "dep-b cloned" test -d lib/dep-b
  assert "dep-a cloned transitively" test -d lib/dep-a
  assert_contains "lock has dep-b" lib/lock.sp 'dep-b.git'
  assert_contains "lock has dep-a" lib/lock.sp 'dep-a.git'
}

# T8: `spcd update` re-resolves and rewrites lock
test_update() {
  echo "T8: spcd update"
  fresh_sandbox
  "$SPCD" add "$FIXDIR/test-repo.git" > /dev/null 2>&1
  # Clobber lock with bogus content; update should replace it.
  echo '/tmp/fake fakesha' > lib/lock.sp
  "$SPCD" update > /dev/null 2>&1
  assert_contains "update rewrote lock" lib/lock.sp '0c7e6d3d9794a92ede8dfa53040de810dd3f6e7a'
}

# T9: `spcd install URL` — compiles main.sp + drops binary into $SPACELANG_BIN
test_install() {
  echo "T9: spcd install"
  fresh_sandbox
  export SPACELANG_BIN="$SANDBOX/bin"
  export SPACELANG_ROOT="$ROOT"
  PATH=$ROOT:$PATH "$SPCD" install "$FIXDIR/binrepo.git" > /dev/null 2>&1
  unset SPACELANG_BIN SPACELANG_ROOT
  assert "installed binary exists" test -x "$SANDBOX/bin/binrepo"
  if [ -x "$SANDBOX/bin/binrepo" ]; then
    local out
    out=$("$SANDBOX/bin/binrepo" 2>&1)
    if echo "$out" | grep -qF 'hello from binrepo'; then
      echo "  $(green PASS) installed binary runs"; PASS=$((PASS+1))
    else
      echo "  $(red FAIL) binary ran but missing expected output:"
      echo "$out" | sed 's/^/      /'
      FAIL=$((FAIL+1)); FAILED_NAMES+=("T9:run")
    fi
  fi
}

# --------------------------------------------------------------------
# Main
# --------------------------------------------------------------------
main() {
  if [ ! -x "$SPCD" ]; then
    echo "spcd binary missing; build it: ./spcc --as spcd spcd.sp -o spcd"
    exit 1
  fi
  setup_fixtures

  test_add_head
  test_list
  test_fetch_idempotent
  test_clean
  test_add_branch
  test_add_sha
  test_transitive
  test_update
  test_install

  echo
  echo "Total: $((PASS+FAIL))  $(green "Pass: $PASS")  $(red "Fail: $FAIL")"
  if [ "$FAIL" -gt 0 ]; then
    printf 'Failed: %s\n' "${FAILED_NAMES[*]}"
    exit 1
  fi
}

main "$@"
