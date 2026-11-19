#!/usr/bin/env bash
# Test suite for canonical bare-hub git-borrow.
# Run: bash bin/test/test-git-borrow.sh [name-substring ...]
# KEEP=1 bash bin/test/test-git-borrow.sh  -> keep temp dirs instead of deleting.
set -u

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
export PATH="$SCRIPT_DIR/..:$SCRIPT_DIR:/usr/bin:$PATH"
KEEP=${KEEP:-0}
RUN_FILTERS=("$@")

FAILURES=()
PASS_COUNT=0
TEST_COUNT=0
CURRENT_TEST=""
ALL_TEMP_DIRS=()
BASE_FIXTURE=""

cleanup_all() {
    [ "$KEEP" -eq 1 ] && [ "${#ALL_TEMP_DIRS[@]}" -gt 0 ] && {
        printf 'KEEP=1: preserving %d temp dir(s):\n' "${#ALL_TEMP_DIRS[@]}" >&2
        printf '  %s\n' "${ALL_TEMP_DIRS[@]}" >&2
        return
    }
    [ "${#ALL_TEMP_DIRS[@]}" -eq 0 ] || rm -rf "${ALL_TEMP_DIRS[@]}"
}
trap cleanup_all EXIT

fail() {
    FAILURES+=("$CURRENT_TEST: $*")
    printf 'FAIL [%s]: %s\n' "$CURRENT_TEST" "$*" >&2
}

assert_eq() {
    local actual=$1 expected=$2 msg=${3:-}
    [ "$actual" = "$expected" ] || fail "${msg:-assert_eq} (expected [$expected], got [$actual])"
}

assert_ne() {
    local actual=$1 unexpected=$2 msg=${3:-}
    [ "$actual" != "$unexpected" ] || fail "${msg:-assert_ne} (both were [$actual])"
}

assert_contains() {
    local haystack=$1 needle=$2 msg=${3:-}
    case "$haystack" in
        *"$needle"*) ;;
        *) fail "${msg:-assert_contains} (did not find [$needle] in [$haystack])" ;;
    esac
}

assert_not_contains() {
    local haystack=$1 needle=$2 msg=${3:-}
    case "$haystack" in
        *"$needle"*) fail "${msg:-assert_not_contains} (unexpectedly found [$needle])" ;;
    esac
}

assert_file_exists() {
    [ -e "$1" ] || fail "${2:-assert_file_exists}: $1 missing"
}

assert_file_absent() {
    [ ! -e "$1" ] || fail "${2:-assert_file_absent}: $1 unexpectedly exists"
}

assert_true_msg() {
    local msg=$1
    shift
    [ "$1" = -- ] && shift
    "$@" || fail "$msg"
}

assert_exit0() {
    local code=$1 msg=${2:-}
    [ "$code" -eq 0 ] || fail "${msg:-assert_exit0} (exit code was $code; err: ${BORROW_ERR:-}; out: ${BORROW_OUT:-})"
}

assert_exit_nonzero() {
    local code=$1 msg=${2:-}
    [ "$code" -ne 0 ] || fail "${msg:-assert_exit_nonzero} (exit code was 0; out: ${BORROW_OUT:-}; err: ${BORROW_ERR:-})"
}

run_borrow() {
    local out err code
    out=$(mktemp) err=$(mktemp)
    git-borrow "$@" >"$out" 2>"$err" </dev/null
    code=$?
    BORROW_OUT=$(cat "$out")
    BORROW_ERR=$(cat "$err")
    BORROW_CODE=$code
    rm -f "$out" "$err"
}

run_borrow_in() {
    local dir=$1 out err code
    shift
    out=$(mktemp) err=$(mktemp)
    ( cd "$dir" && git-borrow "$@" ) >"$out" 2>"$err" </dev/null
    code=$?
    BORROW_OUT=$(cat "$out")
    BORROW_ERR=$(cat "$err")
    BORROW_CODE=$code
    rm -f "$out" "$err"
}

# ----------------------------------------------------------- fixtures -----

build_base_fixture() {
    [ -n "$BASE_FIXTURE" ] && return 0
    BASE_FIXTURE=$(mktemp -d /tmp/borrowhub-base.XXXX)
    command -v cygpath >/dev/null 2>&1 && BASE_FIXTURE=$(cygpath -m "$BASE_FIXTURE")
    ALL_TEMP_DIRS+=("$BASE_FIXTURE")
    local base=$BASE_FIXTURE
    mkdir -p "$base/up" "$base/src"

    local saved_global=${GIT_CONFIG_GLOBAL:-} saved_nosystem=${GIT_CONFIG_NOSYSTEM:-}
    export GIT_CONFIG_GLOBAL="$base/fixture-gitconfig"
    export GIT_CONFIG_NOSYSTEM=1
    cat > "$GIT_CONFIG_GLOBAL" <<EOF
[user]
	name = T
	email = t@e
[init]
	defaultBranch = main
[protocol "file"]
	allow = always
[url "file://$base/up/"]
	insteadOf = https://example.test/org/
EOF

    git init -q "$base/src/subsub"
    ( cd "$base/src/subsub" && head -c 50000 /dev/urandom > blob.bin && echo one > file.txt \
        && git add -A && git commit -q -m "subsub c1" )
    git clone -q --bare "$base/src/subsub" "$base/up/subsub.git"

    git init -q "$base/src/sub"
    ( cd "$base/src/sub" && head -c 50000 /dev/urandom > blob.bin && echo one > file.txt \
        && git submodule add -q https://example.test/org/subsub.git nested \
        && git add -A && git commit -q -m "sub c1" )
    git clone -q --bare "$base/src/sub" "$base/up/sub.git"

    git init -q "$base/src/parent"
    ( cd "$base/src/parent" && head -c 50000 /dev/urandom > blob.bin && echo one > file.txt \
        && git submodule add -q --name lib/sub https://example.test/org/sub.git lib/sub \
        && git add -A && git commit -q -m "parent c1" )
    git clone -q --bare "$base/src/parent" "$base/up/parent.git"

    if [ -n "$saved_global" ]; then export GIT_CONFIG_GLOBAL=$saved_global; else unset GIT_CONFIG_GLOBAL; fi
    if [ -n "$saved_nosystem" ]; then export GIT_CONFIG_NOSYSTEM=$saved_nosystem; else unset GIT_CONFIG_NOSYSTEM; fi
}

new_env() {
    build_base_fixture
    T=$(mktemp -d /tmp/borrowhub.XXXX)
    command -v cygpath >/dev/null 2>&1 && T=$(cygpath -m "$T")
    ALL_TEMP_DIRS+=("$T")
    cp -r "$BASE_FIXTURE/up" "$T/up"
    export GIT_CONFIG_GLOBAL="$T/gitconfig"
    export GIT_CONFIG_NOSYSTEM=1
    cat > "$GIT_CONFIG_GLOBAL" <<EOF
[user]
	name = T
	email = t@e
[init]
	defaultBranch = main
[protocol "file"]
	allow = always
[url "file://$T/up/"]
	insteadOf = https://example.test/org/
	insteadOf = https://EXAMPLE.test/org/
	insteadOf = git@mirror.test:other/
[uploadpack]
	allowFilter = true
	allowAnySHA1InWant = true
[gc]
	auto = 0
[maintenance]
	auto = false
[borrow]
	hubBase = $T/hubs
EOF
    mkdir -p "$T/work"
}

breadcrumb_borrower_id() {
    sed -n 's/^id=//p' "$(git -C "$1" rev-parse --path-format=absolute --git-common-dir)/borrow-origin" | tail -n 1
}

objects_kib_of() {
    git -C "$1" count-objects -v | awk '/^size: |^size-pack: /{s+=$2} END{print s+0}'
}

# ------------------------------------------------------------- runner -----

should_run() {
    local name=$1
    [ "${#RUN_FILTERS[@]}" -eq 0 ] && return 0
    local f
    for f in "${RUN_FILTERS[@]}"; do
        case "$name" in *"$f"*) return 0 ;; esac
    done
    return 1
}

run_test() {
    local name=$1 fn=$2
    should_run "$name" || return 0
    TEST_COUNT=$((TEST_COUNT + 1))
    CURRENT_TEST=$name
    local before=${#FAILURES[@]}
    local t0 t1
    t0=$(date +%s)
    "$fn"
    t1=$(date +%s)
    if [ "${#FAILURES[@]}" -eq "$before" ]; then
        PASS_COUNT=$((PASS_COUNT + 1))
        printf 'PASS %s (%ds)\n' "$name" "$((t1 - t0))"
    else
        printf 'DONE %s (%ds, with failures above)\n' "$name" "$((t1 - t0))"
    fi
    [ "$KEEP" -eq 1 ] || rm -rf "$T"
}

# ---------------------------------------------------------------- tests ---

# (1) Bare hub creation via `clone`: deterministic <hubBase>/<host>/<path>.git,
# zero-collision checkout of default branch `main` tracking `origin/main`,
# clean `git remote`, recursive submodule borrowing, and second clone behavior.
test_clone_bare_hub_and_worktrees() {
    new_env
    local hub="$T/hubs/example.test/org/parent.git"
    local sub_hub="$T/hubs/example.test/org/sub.git"
    local subsub_hub="$T/hubs/example.test/org/subsub.git"

    run_borrow clone -r https://example.test/org/parent.git "$T/work/wt1"
    assert_exit0 "$BORROW_CODE" "clone -r should succeed ($BORROW_ERR)"

    assert_file_exists "$hub/HEAD" "bare hub should be created at canonical host/path.git"
    assert_eq "$(git -C "$hub" config --bool core.bare)" "true" "hub should be bare"
    assert_eq "$(git -C "$hub" config --bool borrow.isHub)" "true" "hub should be marked borrow.isHub"
    assert_file_exists "$sub_hub/HEAD" "submodule bare hub should be auto-created"
    assert_file_exists "$subsub_hub/HEAD" "nested submodule bare hub should be auto-created"

    # First clone checks out default branch `main` with upstream tracking and NO detach warning!
    assert_eq "$(git -C "$T/work/wt1" symbolic-ref --short HEAD)" "main" "first clone should check out main"
    assert_eq "$(git -C "$T/work/wt1" rev-parse --abbrev-ref '@{u}')" "origin/main" "main should track origin/main"
    assert_not_contains "$BORROW_ERR" "detached" "first clone of a bare hub should not collide on main"
    assert_file_exists "$T/work/wt1/lib/sub/nested/file.txt" "nested submodule should be populated"

    # Hub remotes are clean of any borrower/* entries.
    assert_eq "$(git -C "$T/work/wt1" remote)" "origin" "worktree git remote should only show real remotes"

    # Second clone without -b gets dir-name branch; explicit -b main falls back to --detach.
    run_borrow clone https://example.test/org/parent.git "$T/work/wt2"
    assert_exit0 "$BORROW_CODE" "second clone should succeed ($BORROW_ERR)"
    assert_eq "$(git -C "$T/work/wt2" symbolic-ref --short HEAD)" "wt2" "second clone should get dir-name branch"

    run_borrow clone -b main https://example.test/org/parent.git "$T/work/wt3"
    assert_exit0 "$BORROW_CODE" "colliding -b main should fall back to --detach ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "already checked out elsewhere; leaving" "should warn on branch collision"

    # Status on worktree & submodule.
    run_borrow status "$T/work/wt1"
    assert_exit0 "$BORROW_CODE" "status on worktree should succeed"
    assert_contains "$BORROW_OUT" "Role: worktree of hub $hub" "status should show worktree role"
    assert_contains "$BORROW_OUT" "origin  https://example.test/org/parent.git" "status should show hub remotes"
    assert_contains "$BORROW_OUT" "lib/sub: borrower of $sub_hub" "status should show submodule borrower"
}
run_test clone_bare_hub_and_worktrees test_clone_bare_hub_and_worktrees

# (2) Multi-remote hub unification & canonical URL normalization:
# URL spelling variations resolve automatically to one canonical hub;
# --hub unifies a second remote (e.g. fork/mirror) onto the same bare hub;
# borrower -> hub absorbs borrower remotes without injecting hub remotes back
# into the borrower, and `status` shows available hub remotes on the borrower.
test_multi_remote_hub_and_status_remotes() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"

    # Case-varied HTTPS spelling maps to the exact same canonical hub.
    run_borrow clone https://EXAMPLE.test/org/subsub.git "$T/work/wt_a"
    assert_exit0 "$BORROW_CODE" "clone with uppercase host should succeed ($BORROW_ERR)"
    assert_file_exists "$hub/HEAD" "canonical lowercase hub path should be created"

    # Unify a mirror/fork URL onto the same hub via --hub <existing-url>.
    run_borrow clone --hub https://example.test/org/subsub.git git@mirror.test:other/subsub.git "$T/work/wt_mirror"
    assert_exit0 "$BORROW_CODE" "clone --hub with second URL should succeed ($BORROW_ERR)"
    assert_file_absent "$T/hubs/mirror.test" "should reuse existing hub instead of creating a second one"
    assert_eq "$(git -C "$T/work/wt_a" config --get remote.other.url)" "git@mirror.test:other/subsub.git" \
        "linked worktrees should automatically share the newly added hub remote"

    # Subsequent clone of the mirror URL resolves to the unified hub automatically (no --hub needed).
    run_borrow hub git@mirror.test:other/subsub.git
    assert_exit0 "$BORROW_CODE" "hub lookup for mapped mirror URL should succeed"
    assert_eq "$BORROW_OUT" "$hub" "mapped mirror URL should resolve directly to the unified hub"

    # Link a standalone borrower (--force) with its own extra remote:
    # hub absorbs borrower's remote, borrower does NOT get hub's extra remotes,
    # and `status` on borrower lists hub remotes with [present] / [available: ...].
    git clone -q https://example.test/org/subsub.git "$T/work/standalone"
    git -C "$T/work/standalone" remote add personal git@personal.invalid:david/subsub.git
    run_borrow link --force "$T/work/standalone"
    assert_exit0 "$BORROW_CODE" "link --force should succeed ($BORROW_ERR)"

    assert_contains "$(git -C "$hub" config --get-regexp '^remote\..*\.url$')" "git@personal.invalid:david/subsub.git" \
        "hub should absorb borrower's non-local remote"
    assert_not_contains "$(git -C "$T/work/standalone" config --get-regexp '^remote\..*\.url$')" "git@mirror.test:other/subsub.git" \
        "borrower should NOT have hub remotes automatically injected"

    run_borrow status "$T/work/standalone"
    assert_exit0 "$BORROW_CODE" "status on borrower should succeed"
    assert_contains "$BORROW_OUT" "Hub remotes:" "borrower status should list hub remotes"
    assert_contains "$BORROW_OUT" "https://EXAMPLE.test/org/subsub.git  [present]" "origin should show [present]"
    assert_contains "$BORROW_OUT" "git@mirror.test:other/subsub.git  [available: git remote add " \
        "missing hub remote should show [available: git remote add ...]"
}
run_test multi_remote_hub_and_status_remotes test_multi_remote_hub_and_status_remotes

# (3) `link` standalone guard: non-submodule standalone repos are rejected
# without --force; --force links via alternates, repacks cleanly with progress
# stats, drops stale MIDX, passes fsck, and is idempotent.
test_link_standalone_guard_and_dedupe() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/work/standalone"
    git -C "$T/work/standalone" repack -a -d -q
    git -C "$T/work/standalone" multi-pack-index write
    assert_file_exists "$T/work/standalone/.git/objects/pack/multi-pack-index" "setup: MIDX should exist"

    # Without --force: refused!
    run_borrow link "$T/work/standalone"
    assert_exit_nonzero "$BORROW_CODE" "link on standalone repo without --force should fail"
    assert_contains "$BORROW_ERR" "standalone repository, not a submodule" "should explain why standalone link failed"
    assert_contains "$BORROW_ERR" "--force" "should mention --force escape hatch"

    local before_kib
    before_kib=$(objects_kib_of "$T/work/standalone")

    # With --force: succeeds, shows progress/savings, shrinks local objects, removes MIDX.
    run_borrow link --force "$T/work/standalone"
    assert_exit0 "$BORROW_CODE" "link --force should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "Repacking " "non-quiet dedupe should show repack progress header"
    assert_contains "$BORROW_ERR" "saved " "dedupe summary should report KiB saved"

    local after_kib
    after_kib=$(objects_kib_of "$T/work/standalone")
    [ "$after_kib" -lt $((before_kib / 4)) ] || fail "dedupe should shrink objects (before=$before_kib after=$after_kib)"
    assert_file_absent "$T/work/standalone/.git/objects/pack/multi-pack-index" "repack -A -d -l should drop stale MIDX"
    assert_true_msg "standalone borrower should pass fsck" -- \
        bash -c 'git -C "$1" fsck --connectivity-only --no-progress >/dev/null 2>&1' _ "$T/work/standalone"

    # Idempotent re-link with --force.
    run_borrow link --force "$T/work/standalone"
    assert_exit0 "$BORROW_CODE" "idempotent re-link should succeed ($BORROW_ERR)"
    assert_eq "$(objects_kib_of "$T/work/standalone")" "$after_kib" "re-link should not change size"

    # Linking a worktree of a hub (no --force needed) is a friendly no-op.
    run_borrow clone https://example.test/org/sub.git "$T/work/wt_ok"
    assert_exit0 "$BORROW_CODE" "clone should succeed"
    run_borrow link "$T/work/wt_ok"
    assert_exit0 "$BORROW_CODE" "link on a hub worktree should be a clean no-op ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "already a worktree of hub" "should note it is already a worktree"
}
run_test link_standalone_guard_and_dedupe test_link_standalone_guard_and_dedupe

# (4) Canonical precondition refusals: partial clones, foreign alternates,
# .keep packs, and reftable repos fail fast with clear diagnostics.
test_canonical_precondition_refusals() {
    new_env
    # Partial clone
    git clone -q --filter=blob:none https://example.test/org/subsub.git "$T/work/partial"
    run_borrow link --force "$T/work/partial"
    assert_exit_nonzero "$BORROW_CODE" "partial clone should be refused"
    assert_contains "$BORROW_ERR" "partial clone" "error should explain partial clones are unsupported"

    # Foreign alternate
    git clone -q https://example.test/org/subsub.git "$T/work/donor"
    git clone -q https://example.test/org/subsub.git "$T/work/foreign_alt"
    echo "$T/work/donor/.git/objects" > "$T/work/foreign_alt/.git/objects/info/alternates"
    run_borrow link --force "$T/work/foreign_alt"
    assert_exit_nonzero "$BORROW_CODE" "foreign alternates should be refused"
    assert_contains "$BORROW_ERR" "foreign alternates" "error should mention foreign alternates"

    # .keep pack
    git clone -q https://example.test/org/subsub.git "$T/work/kept"
    git -C "$T/work/kept" repack -a -d -q
    local pack
    pack=$(ls "$T/work/kept/.git/objects/pack"/pack-*.pack | head -n 1)
    : > "${pack%.pack}.keep"
    run_borrow link --force "$T/work/kept"
    assert_exit_nonzero "$BORROW_CODE" ".keep pack should be refused"
    assert_contains "$BORROW_ERR" ".keep" "error should mention .keep pack"

    # Reftable (if git supports --ref-format=reftable)
    if git init -q --ref-format=reftable "$T/work/rt" 2>/dev/null; then
        git -C "$T/work/rt" remote add origin https://example.test/org/subsub.git
        git -C "$T/work/rt" fetch -q origin
        git -C "$T/work/rt" checkout -q -b main origin/main
        run_borrow link --force "$T/work/rt"
        assert_exit_nonzero "$BORROW_CODE" "reftable repo should be refused"
        assert_contains "$BORROW_ERR" "reftable" "error should mention reftable"
    fi
}
run_test canonical_precondition_refusals test_canonical_precondition_refusals

# (5) Dedupe preserves reflog-only commits, staged-only blobs, and
# borrower-local remote-tracking refs (not mirrored into hub).
test_dedupe_preserves_local_only_objects() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"
    run_borrow hub https://example.test/org/subsub.git
    assert_exit0 "$BORROW_CODE" "hub creation should succeed"

    # Push a branch upstream AFTER hub creation so hub lacks it.
    git clone -q https://example.test/org/subsub.git "$T/pusher"
    ( cd "$T/pusher" && git checkout -q -b extra \
        && head -c 7000 /dev/urandom > extra.bin && git add -A \
        && git commit -q -m "extra commit" && git push -q origin extra:extra )
    local extra_oid
    extra_oid=$(git -C "$T/pusher" rev-parse extra)

    git clone -q https://example.test/org/subsub.git "$T/work/b"
    git -C "$T/work/b" fetch -q origin extra:refs/remotes/origin/extra

    # Reflog-only commit (amended away) + staged-only blob.
    ( cd "$T/work/b" && echo reflog-only > reflog.txt && git add reflog.txt \
        && git commit -q -m "orphan me" && git commit -q --amend -m "amended" )
    local reflog_oid staged_oid
    reflog_oid=$(git -C "$T/work/b" log -g --format=%H | sed -n 2p)
    echo staged-only > "$T/work/b/staged.txt"
    git -C "$T/work/b" add staged.txt
    staged_oid=$(git -C "$T/work/b" rev-parse :staged.txt)

    run_borrow link --force "$T/work/b"
    assert_exit0 "$BORROW_CODE" "link --force should succeed ($BORROW_ERR)"

    assert_true_msg "reflog-only commit should survive repack -A -d -l" -- git -C "$T/work/b" cat-file -e "$reflog_oid"
    assert_true_msg "staged-only blob should survive repack -A -d -l" -- git -C "$T/work/b" cat-file -e "$staged_oid"
    assert_true_msg "borrower remote-tracking commit should remain locally" -- \
        env GIT_NO_LAZY_FETCH=1 git -C "$T/work/b" cat-file -e "$extra_oid"
    assert_true_msg "hub must NOT mirror borrower's refs/remotes/* commit" -- \
        bash -c '! git -C "$1" cat-file -e "$2" 2>/dev/null' _ "$hub" "$extra_oid"
    assert_true_msg "borrower should stay fsck-clean" -- \
        bash -c 'git -C "$1" fsck --connectivity-only --no-progress >/dev/null 2>&1' _ "$T/work/b"
}
run_test dedupe_preserves_local_only_objects test_dedupe_preserves_local_only_objects

# (6) Hub GC safety, pre-auto-gc hook, and broken-borrower fault isolation.
test_hub_gc_safety_and_sync_isolation() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"
    git clone -q https://example.test/org/subsub.git "$T/work/b1"
    run_borrow link --force "$T/work/b1"
    assert_exit0 "$BORROW_CODE" "link --force b1 should succeed ($BORROW_ERR)"

    assert_file_exists "$hub/hooks/pre-auto-gc" "pre-auto-gc hook should be installed in bare hub"
    assert_true_msg "pre-auto-gc hook should be executable" -- test -x "$hub/hooks/pre-auto-gc"
    assert_eq "$(git -C "$hub" remote)" "origin" "hub git remote should not contain borrower/*"

    # Create a detached commit in b1 only reachable via b1's detached HEAD, sync,
    # then gc --prune=now on hub: commit must survive!
    ( cd "$T/work/b1" && git checkout -q --detach HEAD && echo det > det.txt \
        && git add det.txt && git commit -q -m "detached X" )
    local X id1
    X=$(git -C "$T/work/b1" rev-parse HEAD)
    id1=$(breadcrumb_borrower_id "$T/work/b1")

    run_borrow gc "$hub" -- --prune=now
    assert_exit0 "$BORROW_CODE" "git borrow gc should succeed ($BORROW_ERR)"
    assert_true_msg "detached commit X should be synced and kept in hub" -- git -C "$hub" cat-file -e "$X"
    assert_true_msg "X should be readable from b1" -- git -C "$T/work/b1" cat-file -e "$X"

    # Hook refuses under GIT_BORROW_SYNCING=1, succeeds and advances lastSync otherwise.
    local old_ls new_ls
    old_ls=$(git -C "$hub" config --get "borrower.$id1.lastSync")
    ( cd "$hub" && GIT_BORROW_SYNCING=1 ./hooks/pre-auto-gc )
    assert_ne "$?" 0 "hook must fail under GIT_BORROW_SYNCING=1"
    sleep 1
    ( cd "$hub" && ./hooks/pre-auto-gc )
    assert_eq "$?" 0 "hook should run sync on bare hub"
    new_ls=$(git -C "$hub" config --get "borrower.$id1.lastSync")
    assert_ne "$new_ls" "$old_ls" "hook sync should advance borrower.<id>.lastSync"

    # Broken second borrower: sync warns and fails overall, healthy b1 still syncs, gc refuses to run.
    git clone -q https://example.test/org/subsub.git "$T/work/b_bad"
    run_borrow link --force "$T/work/b_bad"
    assert_exit0 "$BORROW_CODE" "link --force b_bad should succeed"
    local id_bad
    id_bad=$(breadcrumb_borrower_id "$T/work/b_bad")
    echo 0123456789012345678901234567890123456789 > "$T/work/b_bad/.git/refs/heads/broken"

    local unreachable_oid unreachable_path
    unreachable_oid=$(echo "unreachable gc probe" | git -C "$hub" hash-object -w --stdin)
    unreachable_path="$hub/objects/${unreachable_oid:0:2}/${unreachable_oid:2}"
    touch -d '1 year ago' "$unreachable_path"

    sleep 1
    run_borrow sync "$hub"
    assert_exit_nonzero "$BORROW_CODE" "sync should fail when one borrower is broken"
    assert_contains "$BORROW_ERR" "sync failed for borrower $id_bad" "warning should name broken borrower"
    assert_ne "$(git -C "$hub" config --get "borrower.$id1.lastSync")" "$new_ls" "healthy b1 should still sync"

    run_borrow gc "$hub" -- --prune=now
    assert_exit_nonzero "$BORROW_CODE" "gc must refuse while a borrower fails sync"
    assert_contains "$BORROW_ERR" "not running gc" "should explain gc refusal"
    assert_file_exists "$unreachable_path" "gc must not have run"

    run_borrow forget --force --hub "$hub" "$id_bad"
    assert_exit0 "$BORROW_CODE" "forget --force should succeed"
    run_borrow gc "$hub" -- --prune=now
    assert_exit0 "$BORROW_CODE" "gc should succeed after forgetting broken borrower"
    assert_file_absent "$unreachable_path" "gc should prune unreachable object now"
}
run_test hub_gc_safety_and_sync_isolation test_hub_gc_safety_and_sync_isolation

# (7) `forget`: active-borrower safety check, --force, --keep-refs, archived-only IDs.
test_forget_lifecycle() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"
    git clone -q https://example.test/org/subsub.git "$T/work/b1"
    run_borrow link --force "$T/work/b1"
    assert_exit0 "$BORROW_CODE" "link --force should succeed"
    local id1
    id1=$(breadcrumb_borrower_id "$T/work/b1")

    run_borrow forget --hub "$hub" "$T/work/b1"
    assert_exit_nonzero "$BORROW_CODE" "forget should refuse while still borrowing"
    assert_contains "$BORROW_ERR" "still borrows objects from" "should explain refusal"

    run_borrow forget --keep-refs --hub "$hub" "$T/work/b1"
    assert_exit0 "$BORROW_CODE" "forget --keep-refs should succeed"
    assert_ne "$(git -C "$hub" for-each-ref --count=1 --format=x "refs/borrowers/$id1/")" "" "refs should be kept"

    run_borrow forget --keep-refs --hub "$hub" "$id1"
    assert_exit_nonzero "$BORROW_CODE" "forget --keep-refs on archived-only id should fail"
    assert_contains "$BORROW_ERR" "only archived refs" "should explain archived-only refusal"

    run_borrow forget --hub "$hub" "$id1"
    assert_exit0 "$BORROW_CODE" "forget on archived-only id should delete refs"
    assert_eq "$(git -C "$hub" for-each-ref --count=1 --format=x "refs/borrowers/$id1/")" "" "archived refs should be gone"

    run_borrow forget --hub "$hub" "deadbeefcafe"
    assert_exit_nonzero "$BORROW_CODE" "bogus id should fail"
    assert_contains "$BORROW_ERR" "not a borrower id or repository: deadbeefcafe" "should name bad id"
}
run_test forget_lifecycle test_forget_lifecycle

# (8) `add` + recursion defaults + partial-submodule invariant + local hub offline init.
test_add_recursion_and_offline_submodule_init() {
    new_env
    # Add a second submodule to upstream `sub.git` so we can verify partial-init invariant.
    git clone -q https://example.test/org/sub.git "$T/scratch"
    ( cd "$T/scratch" && git submodule add -q https://example.test/org/subsub.git nested2 \
        && git add -A && git commit -q -m "add nested2" && git push -q origin HEAD:main )

    run_borrow clone https://example.test/org/sub.git "$T/work/wt_src"
    assert_exit0 "$BORROW_CODE" "plain clone should succeed"
    assert_file_absent "$T/work/wt_src/nested/.git" "no -r leaves submodules uninitialized"

    # Initialize ONLY `nested` (not `nested2`), then link the submodule (no --force needed!).
    ( cd "$T/work/wt_src" && git submodule update -q --init -- nested )
    run_borrow link "$T/work/wt_src/nested"
    assert_exit0 "$BORROW_CODE" "link on submodule should succeed without --force ($BORROW_ERR)"

    # Disable subsub upstream so any network clone would fail!
    mv "$T/up/subsub.git" "$T/up/subsub.git.disabled"

    # `add -r` from wt_src should init ONLY `nested` from the local bare hub, leaving `nested2` alone.
    run_borrow_in "$T/work/wt_src" add -r -b topic1 "$T/work/wt_child" main
    assert_exit0 "$BORROW_CODE" "add -r should init nested offline from bare hub ($BORROW_ERR)"
    assert_file_exists "$T/work/wt_child/nested/file.txt" "nested should be populated offline"
    assert_file_absent "$T/work/wt_child/nested2/.git" "uninitialized nested2 must stay uninitialized"
    assert_eq "$(git -C "$T/work/wt_child/nested" config --get remote.origin.url)" \
        "https://example.test/org/subsub.git" "submodule origin should be restored to real URL"

    # Branch collision fallback on `add`.
    run_borrow_in "$T/work/wt_src" add "$T/work/wt_coll" main
    assert_exit0 "$BORROW_CODE" "add with colliding branch should fall back to --detach ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "falling back to --detach" "should warn about --detach fallback"

    # submodule.recurse=true + --no-recursive override.
    mv "$T/up/subsub.git.disabled" "$T/up/subsub.git"
    git -C "$T/work/wt_src" config submodule.recurse true
    run_borrow_in "$T/work/wt_src" add --no-recursive -b norec "$T/work/wt_norec" main
    assert_exit0 "$BORROW_CODE" "--no-recursive should succeed"
    assert_file_absent "$T/work/wt_norec/nested/.git" "--no-recursive should override submodule.recurse=true"
}
run_test add_recursion_and_offline_submodule_init test_add_recursion_and_offline_submodule_init

# (9) `submodule [add]`: from URL, lightweight primary absorption via
# `git submodule absorbgitdirs`, and clear rejection of linked worktree absorb.
test_submodule_add_and_lightweight_absorb() {
    new_env
    local subsub_hub="$T/hubs/example.test/org/subsub.git"
    run_borrow clone https://example.test/org/sub.git "$T/work/super"
    assert_exit0 "$BORROW_CODE" "clone super should succeed"

    # From URL (with -b and --name)
    run_borrow_in "$T/work/super" submodule add -b main --name custom-sub https://example.test/org/subsub.git lib/my-sub
    assert_exit0 "$BORROW_CODE" "submodule add from URL should succeed ($BORROW_ERR)"
    assert_file_exists "$T/work/super/lib/my-sub/file.txt" "submodule checked out"
    assert_contains "$(cat "$(git -C "$T/work/super/lib/my-sub" rev-parse --absolute-git-dir)/objects/info/alternates")" \
        "$subsub_hub/objects" "submodule should borrow from canonical bare hub"
    assert_contains "$(git -C "$T/work/super" diff --cached --name-only)" ".gitmodules" ".gitmodules staged"

    # Lightweight absorb from existing primary standalone clone (dirty + untracked preserved!)
    git clone -q https://example.test/org/subsub.git "$T/work/super/vendor/local-sub"
    echo "dirty edit" >> "$T/work/super/vendor/local-sub/file.txt"
    echo "untracked" > "$T/work/super/vendor/local-sub/new.txt"
    run_borrow_in "$T/work/super" submodule vendor/local-sub
    assert_exit0 "$BORROW_CODE" "lightweight absorb of primary clone should succeed ($BORROW_ERR)"
    assert_true_msg ".git should now be absorbed gitdir link" -- test -f "$T/work/super/vendor/local-sub/.git"
    local sub_status
    sub_status=$(git -C "$T/work/super/vendor/local-sub" status --porcelain)
    assert_contains "$sub_status" "M file.txt" "dirty edit preserved"
    assert_contains "$sub_status" "?? new.txt" "untracked file preserved"
    assert_contains "$(cat "$(git -C "$T/work/super/vendor/local-sub" rev-parse --absolute-git-dir)/objects/info/alternates")" \
        "$subsub_hub/objects" "absorbed submodule should borrow from hub"

    # Linked worktree absorption is explicitly unsupported (fail fast).
    run_borrow clone https://example.test/org/subsub.git "$T/work/super/vendor/linked-wt"
    assert_exit0 "$BORROW_CODE" "setup linked worktree inside super"
    run_borrow_in "$T/work/super" submodule add vendor/linked-wt
    assert_exit_nonzero "$BORROW_CODE" "absorbing a linked worktree should be refused"
    assert_contains "$BORROW_ERR" "cannot absorb linked worktree" "error should explain linked worktree refusal"

    # Unsupported submodule subcommand
    run_borrow_in "$T/work/super" submodule update
    assert_exit_nonzero "$BORROW_CODE" "unsupported submodule subcommand should fail"
    assert_contains "$BORROW_ERR" "unsupported submodule command: update" "should name unsupported subcommand"
}
run_test submodule_add_and_lightweight_absorb test_submodule_add_and_lightweight_absorb

# (10) Shared LFS storage (`lfs.storage`), global status (`-g`), and CLI guards.
test_lfs_shared_storage_global_status_and_cli() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"
    git clone -q https://example.test/org/subsub.git "$T/work/b_lfs"
    mkdir -p "$T/work/b_lfs/.git/lfs/objects/ab"
    printf 'fake-lfs-blob' > "$T/work/b_lfs/.git/lfs/objects/ab/item.bin"

    run_borrow link --force "$T/work/b_lfs"
    assert_exit0 "$BORROW_CODE" "link --force with LFS objects should succeed ($BORROW_ERR)"
    assert_eq "$(git -C "$T/work/b_lfs" config --get lfs.storage)" "$hub/lfs" "borrower lfs.storage should point at hub/lfs"
    assert_file_exists "$hub/lfs/objects/ab/item.bin" "existing borrower LFS object should be migrated into hub/lfs"

    # Global status shows mappings, hub remotes, worktrees, and borrowers.
    run_borrow clone https://example.test/org/subsub.git "$T/work/wt_g"
    assert_exit0 "$BORROW_CODE" "clone for global status should succeed"
    run_borrow status -g
    assert_exit0 "$BORROW_CODE" "status -g should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_OUT" "Global hub mappings:" "header present"
    assert_contains "$BORROW_OUT" "example.test/org/subsub -> $hub" "canonical mapping shown"
    assert_contains "$BORROW_OUT" "Remotes:" "remotes section shown"
    assert_contains "$BORROW_OUT" "$T/work/wt_g" "linked worktree shown"
    assert_contains "$BORROW_OUT" "$T/work/b_lfs" "borrower shown"

    # Unknown command rejected.
    run_borrow map
    assert_exit_nonzero "$BORROW_CODE" "unknown command should fail"
    assert_contains "$BORROW_ERR" "unknown command" "should report unknown command"
}
run_test lfs_shared_storage_global_status_and_cli test_lfs_shared_storage_global_status_and_cli

# ------------------------------------------------------------- summary ----

printf '\n==== %d/%d tests passed ====\n' "$PASS_COUNT" "$TEST_COUNT"
if [ "${#FAILURES[@]}" -gt 0 ]; then
    printf 'Failures:\n'
    printf '  %s\n' "${FAILURES[@]}"
    exit 1
fi
exit 0
