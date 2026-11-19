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
    assert_not_contains "$BORROW_ERR" "Traceback (most recent call last):" "unexpected Python traceback on stderr"
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
    assert_not_contains "$BORROW_ERR" "Traceback (most recent call last):" "unexpected Python traceback on stderr"
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
    assert_eq "$(git -C "$hub" config --local --bool uploadpack.allowAnySHA1InWant)" "true" \
        "hub should enable uploadpack.allowAnySHA1InWant locally"
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
    assert_eq "$(git -C "$T/work/wt3" rev-parse --abbrev-ref HEAD)" "HEAD" "wt3 HEAD should actually be detached"

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
# and .keep packs fail fast with clear diagnostics.
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
    # C1 safety: borrower's refs/remotes/* is pinned under refs/borrowers/<id>/refs/*
    # without polluting hub's own heads/remotes, and survives hub fetch --prune && gc.
    assert_true_msg "hub must NOT create top-level branch for borrower extra ref" -- \
        bash -c '! git -C "$1" show-ref --verify --quiet refs/heads/extra' _ "$hub"
    assert_true_msg "borrower ref must remain reachable across hub prune + gc" -- \
        bash -c '
            git -C "$1" push -q origin --delete extra
            git -C "$2" fetch -q --prune
            git -C "$2" gc -q --prune=now
            GIT_NO_LAZY_FETCH=1 git -C "$3" cat-file -e "$4"
        ' _ "$T/pusher" "$hub" "$T/work/b" "$extra_oid"
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
    assert_eq "$(git -C "$T/work/wt_coll" rev-parse --abbrev-ref HEAD)" "HEAD" "wt_coll HEAD should actually be detached"

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
    local before_kib after_kib sub_gitdir staged_names
    before_kib=$(du -sk "$T/work/super/vendor/local-sub/.git/objects" | awk '{print $1}')
    run_borrow_in "$T/work/super" submodule vendor/local-sub
    assert_exit0 "$BORROW_CODE" "lightweight absorb of primary clone should succeed ($BORROW_ERR)"
    assert_true_msg ".git should now be absorbed gitdir link" -- test -f "$T/work/super/vendor/local-sub/.git"
    local sub_status
    sub_status=$(git -C "$T/work/super/vendor/local-sub" status --porcelain)
    assert_contains "$sub_status" "M file.txt" "dirty edit preserved"
    assert_contains "$sub_status" "?? new.txt" "untracked file preserved"
    sub_gitdir=$(git -C "$T/work/super/vendor/local-sub" rev-parse --absolute-git-dir)
    assert_contains "$(cat "$sub_gitdir/objects/info/alternates")" \
        "$subsub_hub/objects" "absorbed submodule should borrow from hub"
    after_kib=$(du -sk "$sub_gitdir/objects" | awk '{print $1}')
    assert_true_msg "absorbed submodule objects dir should shrink ($after_kib KiB vs $before_kib KiB)" -- \
        test "$after_kib" -lt "$before_kib"
    staged_names=$(git -C "$T/work/super" diff --cached --name-only)
    assert_contains "$staged_names" ".gitmodules" "superproject should stage .gitmodules on absorb"
    assert_contains "$staged_names" "vendor/local-sub" "superproject should stage absorbed gitlink"

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

# (11) Hook isolation (pre-auto-gc), active gc.pid guard, and non-ff/hook sync resilience.
test_sync_safety_hooks_gc_pid_and_deny_non_ff() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"
    run_borrow hub https://example.test/org/subsub.git
    assert_exit0 "$BORROW_CODE" "hub creation should succeed"

    # pre-auto-gc must block auto-gc during GIT_BORROW_SYNCING=1, allow normal runs, and never loop on gc.auto=1.
    git -C "$hub" config gc.auto 1
    assert_true_msg "pre-auto-gc should exit 0 normally" -- bash -c '( cd "$1" && ./hooks/pre-auto-gc )' _ "$hub"
    assert_true_msg "pre-auto-gc should exit 1 when GIT_BORROW_SYNCING=1" -- \
        bash -c '! ( cd "$1" && GIT_BORROW_SYNCING=1 ./hooks/pre-auto-gc )' _ "$hub"
    run_borrow sync "$hub"
    assert_exit0 "$BORROW_CODE" "sync with gc.auto=1 should complete cleanly without recursion ($BORROW_ERR)"

    # Active gc.pid on matching host must block borrow gc; dead PID must not block or crash.
    local bg_sh_pid bg_pid bg_host
    rm -f "$T/bg_gc.info" "$T/bg_gc.stop"
    python3 -c 'import os, socket, sys, time
info, stop = sys.argv[1], sys.argv[2]
with open(info, "w") as f:
    f.write(f"{os.getpid()} {socket.gethostname()}\n")
deadline = time.time() + 15
while time.time() < deadline and not os.path.exists(stop):
    time.sleep(0.05)' "$T/bg_gc.info" "$T/bg_gc.stop" &
    bg_sh_pid=$!
    while [ ! -s "$T/bg_gc.info" ]; do sleep 0.05; done
    read -r bg_pid bg_host < "$T/bg_gc.info"
    printf '%s %s\n' "$bg_pid" "$bg_host" > "$hub/gc.pid"
    run_borrow gc "$hub"
    touch "$T/bg_gc.stop"
    wait "$bg_sh_pid" 2>/dev/null || true
    assert_exit_nonzero "$BORROW_CODE" "gc must refuse while gc.pid process is alive"
    assert_contains "$BORROW_ERR" "active git gc" "should report active gc.pid guard"

    printf '99999999 %s\n' "$bg_host" > "$hub/gc.pid"
    run_borrow gc "$hub"
    rm -f "$hub/gc.pid"
    assert_exit0 "$BORROW_CODE" "stale gc.pid must not block or crash gc ($BORROW_ERR)"

    # receive.denyNonFastForwards + rejecting pre-receive hook on hub must not block link --force sync.
    git -C "$hub" config receive.denyNonFastForwards true
    cat > "$hub/hooks/pre-receive" << 'EOF'
#!/usr/bin/env sh
exit 1
EOF
    chmod +x "$hub/hooks/pre-receive"

    git clone -q https://example.test/org/subsub.git "$T/work/b_div"
    ( cd "$T/work/b_div" && git checkout -q --orphan diverged \
        && echo "diverged" > div.txt && git add div.txt && git commit -q -m "orphan diverged" )
    run_borrow link --force "$T/work/b_div"
    assert_exit0 "$BORROW_CODE" "link --force must succeed despite denyNonFastForwards & pre-receive ($BORROW_ERR)"
    local div_oid
    div_oid=$(git -C "$T/work/b_div" rev-parse HEAD)
    assert_true_msg "diverged borrower commit should be synced into hub" -- git -C "$hub" cat-file -e "$div_oid"
}
run_test sync_safety_hooks_gc_pid_and_deny_non_ff test_sync_safety_hooks_gc_pid_and_deny_non_ff

# (12) Atomic hub creation cleanup and mid-dedupe rollback on failure.
test_rollback_and_atomic_hub_creation() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"

    # Bad URL during hub creation leaves zero partial hub, .tmp dirs, or empty host dirs.
    run_borrow clone https://nonexistent.invalid/org/broken.git "$T/work/broken"
    assert_exit_nonzero "$BORROW_CODE" "clone of unreachable URL should fail cleanly"
    assert_file_absent "$T/hubs/nonexistent.invalid" "failed hub creation must prune empty parent dirs and staging dirs"
    assert_file_absent "$T/work/broken" "failed clone must not leave empty destination dir"

    # Unborn-HEAD / empty upstream repo clone fails cleanly and leaves no hub or dir.
    git init -q --bare --initial-branch=main "$T/up/empty.git"
    run_borrow clone https://example.test/org/empty.git "$T/work/empty_dest"
    assert_exit_nonzero "$BORROW_CODE" "cloning empty repo should fail cleanly"
    assert_contains "$BORROW_ERR" "no commits" "should explain empty/unborn upstream refusal"
    assert_file_absent "$T/hubs/example.test/org/empty.git" "failed empty-repo clone must not keep hub"
    assert_file_absent "$T/work/empty_dest" "failed empty-repo clone must not keep dest"

    # Simulate repack failure during dedupe_objects via invalid pack.compression=99.
    git clone -q https://example.test/org/subsub.git "$T/work/b_rb"
    git -C "$T/work/b_rb" config pack.compression 99
    run_borrow link --force "$T/work/b_rb"
    assert_exit_nonzero "$BORROW_CODE" "repack failure during link must fail cleanly"
    assert_file_absent "$T/work/b_rb/.git/objects/info/alternates" "rollback must remove newly written alternates"
    assert_file_absent "$T/work/b_rb/.git/borrow-origin" "rollback must not write borrow-origin breadcrumb"
    assert_eq "$(git -C "$hub" config --get-regexp '^borrower\.' || true)" "" \
        "rollback must remove failed borrower registration from hub"
    git -C "$T/work/b_rb" config --unset pack.compression
    assert_true_msg "borrower objects must stay intact and fsck-clean after rollback" -- \
        bash -c 'git -C "$1" fsck --connectivity-only --no-progress >/dev/null 2>&1' _ "$T/work/b_rb"
}
run_test rollback_and_atomic_hub_creation test_rollback_and_atomic_hub_creation

# (13) Submodule edge cases: occupied dest, worktreeConfig isolation, absorb guards & relative URLs.
test_submodule_edge_cases_and_absorb_guards() {
    new_env
    local sub_hub="$T/hubs/example.test/org/sub.git"
    local subsub_hub="$T/hubs/example.test/org/subsub.git"

    run_borrow clone https://example.test/org/sub.git "$T/work/wt_occ"
    assert_exit0 "$BORROW_CODE" "clone sub should succeed"

    # Pre-existing occupied directory must block offline submodule init without clobbering files.
    mkdir -p "$T/work/wt_occ/nested"
    echo "precious" > "$T/work/wt_occ/nested/keep.txt"
    run_borrow_in "$T/work/wt_occ" submodule add https://example.test/org/subsub.git nested
    assert_exit_nonzero "$BORROW_CODE" "initializing submodule into occupied directory must fail"
    assert_contains "$BORROW_ERR" "is non-empty" "should explain occupied submodule path refusal"
    assert_eq "$(cat "$T/work/wt_occ/nested/keep.txt")" "precious" "existing file untouched"

    # Relative submodule URL (`../subsub.git`) + worktreeConfig isolation.
    run_borrow clone -r https://example.test/org/sub.git "$T/work/wt_rel_src"
    assert_exit0 "$BORROW_CODE" "clone -r src should succeed"
    git -C "$sub_hub" config extensions.worktreeConfig true
    git -C "$T/work/wt_rel_src" config --worktree user.name "WT User"
    run_borrow_in "$T/work/wt_rel_src" add -r -b rel-test "$T/work/wt_rel" main
    assert_exit0 "$BORROW_CODE" "add -r with extensions.worktreeConfig should succeed ($BORROW_ERR)"
    assert_file_exists "$T/work/wt_rel/nested/file.txt" "nested submodule populated"
    assert_not_contains "$(git -C "$sub_hub" config --local --list || true)" "$subsub_hub" \
        "transient submodule hub URL override must not leak into shared hub config"
    assert_not_contains "$(git -C "$T/work/wt_rel" config --worktree --list || true)" "$subsub_hub" \
        "transient submodule hub URL override must not leak into worktree config"

    # Relative URL on `submodule add ../subsub.git` (no bare repo at ../subsub.git inside worktree parent).
    run_borrow_in "$T/work/wt_rel" submodule add ../subsub.git rel-sub
    assert_exit0 "$BORROW_CODE" "submodule add with relative ../subsub.git URL should succeed ($BORROW_ERR)"
    assert_file_exists "$T/work/wt_rel/rel-sub/file.txt" "relative submodule checked out"

    # Absorb guards: outside repo, target '.', bare repo, subdir absorb without double-prefixing, occupied dest, unborn HEAD.
    run_borrow_in "$T" submodule add https://example.test/org/subsub.git sub
    assert_exit_nonzero "$BORROW_CODE" "submodule outside a working tree must fail"

    run_borrow_in "$T/work/wt_rel" submodule .
    assert_exit_nonzero "$BORROW_CODE" "submodule add targeting '.' must fail"

    run_borrow_in "$T/work/wt_rel" submodule "$T/hubs/example.test/org/subsub.git"
    assert_exit_nonzero "$BORROW_CODE" "submodule add with bare repo path must fail"

    git clone -q https://example.test/org/subsub.git "$T/work/outside_sub"
    mkdir -p "$T/work/wt_rel/vendor/existing_dir"
    echo "blocker" > "$T/work/wt_rel/vendor/existing_dir/stay.txt"
    run_borrow_in "$T/work/wt_rel" submodule "$T/work/outside_sub" vendor/existing_dir
    assert_exit_nonzero "$BORROW_CODE" "absorbing into occupied destination must fail"
    assert_true_msg "source repo must remain intact when dest occupied" -- test -d "$T/work/outside_sub/.git"
    assert_eq "$(cat "$T/work/wt_rel/vendor/existing_dir/stay.txt")" "blocker" "occupied dest file preserved"

    # Subdir CWD absorb (avoid double-prefixing sub/sub).
    mkdir -p "$T/work/wt_rel/pkgs"
    git clone -q https://example.test/org/subsub.git "$T/work/wt_rel/pkgs/local_pkg"
    run_borrow_in "$T/work/wt_rel/pkgs" submodule local_pkg
    assert_exit0 "$BORROW_CODE" "absorbing from superproject subdir should succeed ($BORROW_ERR)"
    assert_true_msg "local_pkg absorbed in pkgs/" -- test -f "$T/work/wt_rel/pkgs/local_pkg/.git"

    # Unborn HEAD primary standalone repo refused before moving!
    git init -q --initial-branch=main "$T/work/unborn_sub"
    git -C "$T/work/unborn_sub" remote add origin https://example.test/org/subsub.git
    run_borrow_in "$T/work/wt_rel" submodule "$T/work/unborn_sub" pkgs/moved_unborn
    assert_exit_nonzero "$BORROW_CODE" "unborn HEAD standalone repo must not be absorbed"
    assert_contains "$BORROW_ERR" "unborn HEAD" "should explain unborn HEAD refusal"
    assert_true_msg "unborn repo must not have been moved" -- test -d "$T/work/unborn_sub"
    assert_file_absent "$T/work/wt_rel/pkgs/moved_unborn" "failed absorb must not create dest"
}
run_test submodule_edge_cases_and_absorb_guards test_submodule_edge_cases_and_absorb_guards

# (14) Precondition refusals (shallow, sha256, non-bare --hub, bare link), env isolation, --, and LFS guards.
test_precondition_guards_env_and_paths() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"
    run_borrow hub https://example.test/org/subsub.git
    assert_exit0 "$BORROW_CODE" "pre-create hub"

    # Shallow clone refusal.
    git clone -q --depth=1 "file://$T/up/subsub.git" "$T/work/shallow_b"
    git -C "$T/work/shallow_b" remote set-url origin https://example.test/org/subsub.git
    run_borrow link --force "$T/work/shallow_b"
    assert_exit_nonzero "$BORROW_CODE" "shallow clone must be refused"
    assert_contains "$BORROW_ERR" "shallow" "should explain shallow refusal"

    # SHA-256 repository and SHA-256 --hub mismatch refusal.
    git init -q --object-format=sha256 --initial-branch=main "$T/work/sha256_b"
    ( cd "$T/work/sha256_b" && echo hi > f.txt && git add f.txt && git commit -q -m init )
    git -C "$T/work/sha256_b" remote add origin https://example.test/org/subsub.git
    run_borrow link --force "$T/work/sha256_b"
    assert_exit_nonzero "$BORROW_CODE" "sha256 borrower must be refused"
    assert_contains "$BORROW_ERR" "objectFormat" "should mention objectFormat"

    git init -q --bare --object-format=sha256 "$T/work/sha256_bare.git"
    run_borrow clone --hub "$T/work/sha256_bare.git" https://example.test/org/subsub.git "$T/work/bad_sha256_wt"
    assert_exit_nonzero "$BORROW_CODE" "sha256 --hub must be refused"
    assert_contains "$BORROW_ERR" "objectFormat" "should reject non-sha1 hub"

    # Non-bare repo passed to --hub, bare repo passed to link.
    git clone -q https://example.test/org/subsub.git "$T/work/plain_nonbare"
    run_borrow clone --hub "$T/work/plain_nonbare" https://example.test/org/subsub.git "$T/work/should_fail"
    assert_exit_nonzero "$BORROW_CODE" "non-bare unassociated repo passed to --hub must fail"
    assert_contains "$BORROW_ERR" "must point to a bare hub" "should explain bare hub requirement"

    git clone -q --bare https://example.test/org/subsub.git "$T/work/plain_bare.git"
    run_borrow link --force "$T/work/plain_bare.git"
    assert_exit_nonzero "$BORROW_CODE" "bare repo passed to link must fail"
    assert_contains "$BORROW_ERR" "not a Git worktree" "should refuse bare repo link"

    # GIT_DIR / GIT_WORK_TREE env vars pointing elsewhere must not hijack git-borrow.
    git init -q "$T/work/env_trap"
    run_borrow status "$T/work/env_trap"
    assert_exit0 "$BORROW_CODE" "baseline status ok"
    BORROW_OUT=""
    local out err code
    out=$(mktemp) err=$(mktemp)
    GIT_DIR="$T/work/env_trap/.git" GIT_WORK_TREE="$T/work/env_trap" git-borrow status "$hub" >"$out" 2>"$err" </dev/null
    code=$?
    BORROW_OUT=$(cat "$out")
    BORROW_ERR=$(cat "$err")
    rm -f "$out" "$err"
    assert_exit0 "$code" "GIT_DIR/GIT_WORK_TREE env must not break git-borrow ($BORROW_ERR)"
    assert_contains "$BORROW_OUT" "Role: hub" "status must inspect requested hub, not GIT_DIR trap"

    # cmd_add -q -f -b <branch> -- -weird-dir main.
    run_borrow clone https://example.test/org/subsub.git "$T/work/wt_dash_src"
    assert_exit0 "$BORROW_CODE" "clone wt_dash_src ok"
    run_borrow_in "$T/work/wt_dash_src" add -q -f -b dash-branch -- ../wt_dash_weird/-weird-dir main
    assert_exit0 "$BORROW_CODE" "add -q -f -b ... -- <dash-dir> should succeed ($BORROW_ERR)"
    assert_true_msg "-weird-dir is a git worktree" -- \
        bash -c 'git -C "$1" rev-parse --is-inside-work-tree >/dev/null' _ "$T/work/wt_dash_weird/-weird-dir"

    # has_lfs false-positive checks (# comment / halfsize) and custom lfs.storage refusal.
    git clone -q https://example.test/org/subsub.git "$T/work/b_nolfs"
    cat > "$T/work/b_nolfs/.gitattributes" << 'EOF'
# filter=lfs diff=lfs merge=lfs -text
*.dat halfsize=true
EOF
    ( cd "$T/work/b_nolfs" && git add .gitattributes && git commit -q -m "comment attrs" )
    run_borrow link --force "$T/work/b_nolfs"
    assert_exit0 "$BORROW_CODE" "link --force without real LFS should succeed ($BORROW_ERR)"
    assert_eq "$(git -C "$T/work/b_nolfs" config --local --get lfs.storage || true)" "" \
        "commented/halfsize .gitattributes must not configure lfs.storage"

    git clone -q https://example.test/org/subsub.git "$T/work/b_custlfs"
    printf '*.bin filter=lfs diff=lfs merge=lfs -text\n' > "$T/work/b_custlfs/.gitattributes"
    ( cd "$T/work/b_custlfs" && git add .gitattributes && git commit -q -m "lfs attrs" )
    git -C "$T/work/b_custlfs" config --local lfs.storage "$T/custom_lfs_dir"
    run_borrow link --force "$T/work/b_custlfs"
    assert_exit_nonzero "$BORROW_CODE" "custom lfs.storage must not be silently overwritten"
    assert_contains "$BORROW_ERR" "custom lfs.storage" "should warn/error about custom lfs.storage"
}
run_test precondition_guards_env_and_paths test_precondition_guards_env_and_paths

# (15) RemoteUrl edge cases (.GIT, ?query#frag, .., file://, dotted stem) & sync/gc/forget/CLI errors.
test_remote_url_and_cli_edge_cases() {
    new_env
    local hub="$T/hubs/example.test/org/subsub.git"
    run_borrow hub https://example.test/org/subsub.git
    assert_exit0 "$BORROW_CODE" "create hub"

    # Case-insensitive .GIT and ?query#fragment resolve to canonical hub.
    run_borrow hub "https://EXAMPLE.test/org/subsub.GIT?ref=main#top"
    assert_exit0 "$BORROW_CODE" "hub with .GIT and ?query#frag should resolve existing hub ($BORROW_ERR)"

    # .. traversal in remote URL must be rejected cleanly.
    run_borrow clone "https://example.test/org/../../escape.git" "$T/work/escape"
    assert_exit_nonzero "$BORROW_CODE" ".. traversal URL must be rejected"
    assert_contains "$BORROW_ERR" "traversal" "should report invalid path traversal"

    # file:// local URL rejected for automatic hub derivation.
    run_borrow clone "file://$T/up/subsub.git" "$T/work/local_file_url"
    assert_exit_nonzero "$BORROW_CODE" "file:// URL cannot auto-derive canonical hub"

    # Dotted repo name without .git preserves full name (Path.name, not .stem).
    git clone -q --bare "$T/up/subsub.git" "$T/up/foo.bar.v2"
    ( cd "$T/work" && git-borrow clone "https://example.test/org/foo.bar.v2" ) >/dev/null 2>&1
    assert_exit0 "$?" "clone dotted repo without target dir should succeed"
    assert_file_exists "$T/work/foo.bar.v2/file.txt" "default dir must preserve full dotted repo name"
    assert_file_exists "$T/hubs/example.test/org/foo.bar.v2.git/HEAD" "hub path created cleanly for dotted repo"

    # sync keeps refs cleanly with warning when a borrower checkout is deleted, and exits 1 if an active borrower fails sync.
    git clone -q https://example.test/org/subsub.git "$T/work/b1"
    git clone -q https://example.test/org/subsub.git "$T/work/b2"
    git clone -q https://example.test/org/subsub.git "$T/work/b3"
    run_borrow link --force "$T/work/b1"
    assert_exit0 "$BORROW_CODE" "link b1"
    run_borrow link --force "$T/work/b2"
    assert_exit0 "$BORROW_CODE" "link b2"
    run_borrow link --force "$T/work/b3"
    assert_exit0 "$BORROW_CODE" "link b3"
    rm -rf "$T/work/b1"
    echo 0123456789012345678901234567890123456789 > "$T/work/b3/.git/refs/heads/broken"
    ( cd "$T/work/b2" && echo "b2new" > b2.txt && git add b2.txt && git commit -q -m "b2 commit" )
    local b2_oid
    b2_oid=$(git -C "$T/work/b2" rev-parse HEAD)
    run_borrow sync "$hub"
    assert_exit_nonzero "$BORROW_CODE" "sync with unreachable/broken borrower b3 must return exit 1"
    assert_contains "$BORROW_ERR" "is gone" "should report deleted borrower b1 with warning"
    assert_contains "$BORROW_ERR" "sync failed for borrower" "should report failing borrower b3"
    assert_true_msg "healthy borrower b2 still synced despite missing b1 and broken b3" -- git -C "$hub" cat-file -e "$b2_oid"
    run_borrow forget --force --hub "$hub" "$T/work/b3"
    assert_exit0 "$BORROW_CODE" "cleanup b3"

    # forget guards: nonexistent / partial ID, active worktree without --force, non-hub target.
    local b2_id prefix
    b2_id=$(git -C "$hub" config --get-regexp '^borrower\..*\.common$' | awk '{sub(/^borrower\./,""); sub(/\.common$/,""); print $1}')
    prefix=${b2_id:0:4}
    run_borrow forget --hub "$hub" "$prefix"
    assert_exit_nonzero "$BORROW_CODE" "partial borrower ID must not match"

    run_borrow forget --hub "$hub" "$T/work/b2"
    assert_exit_nonzero "$BORROW_CODE" "forget on existing borrower checkout without --force should fail"
    run_borrow forget --force --hub "$hub" "$T/work/b2"
    assert_exit0 "$BORROW_CODE" "forget --force on existing borrower should succeed ($BORROW_ERR)"

    run_borrow gc
    assert_exit_nonzero "$BORROW_CODE" "gc outside hub/borrower with no arg must fail"

    # Invalid per-command flags rejected by scoped argparse parser.
    run_borrow status --branch main
    assert_exit_nonzero "$BORROW_CODE" "status must reject clone-only --branch flag"
    run_borrow forget -b main something
    assert_exit_nonzero "$BORROW_CODE" "forget must reject -b flag"
    run_borrow hub --detach
    assert_exit_nonzero "$BORROW_CODE" "hub must reject --detach flag"
}
run_test remote_url_and_cli_edge_cases test_remote_url_and_cli_edge_cases

# (16) Reftable hubs, borrowers, mixed backends, pseudo-refs (ORIG_HEAD), and absorbgitdirs.
test_reftable_hubs_and_borrowers() {
    new_env
    if ! git init -q --ref-format=reftable "$T/rt_probe" 2>/dev/null; then
        printf 'SKIP reftable tests (git lacks --ref-format=reftable)\n'
        return
    fi
    rm -rf "$T/rt_probe"

    # (a) Mixed: files hub + reftable standalone borrower (with ORIG_HEAD inside reftable + linked worktree ORIG_HEAD).
    local fhub="$T/hubs/example.test/org/subsub.git"
    git init -q --ref-format=reftable --initial-branch=main "$T/work/rt_b"
    git -C "$T/work/rt_b" remote add origin https://example.test/org/subsub.git
    git -C "$T/work/rt_b" fetch -q origin
    git -C "$T/work/rt_b" reset -q --hard origin/main
    ( cd "$T/work/rt_b" && echo "v1" > orig.txt && git add orig.txt && git commit -q -m "orig1" \
        && git reset -q --hard HEAD^ && git reflog expire --expire=now --all )
    local orig_oid_primary orig_oid_wt
    orig_oid_primary=$(git -C "$T/work/rt_b" rev-parse -q --verify ORIG_HEAD)
    assert_ne "$orig_oid_primary" "" "reftable primary should have ORIG_HEAD"
    # Create a linked reftable worktree on rt_b whose own reftable ORIG_HEAD points to an orphaned commit.
    git -C "$T/work/rt_b" worktree add -q -b rt_side "$T/work/rt_b_wt" origin/main
    ( cd "$T/work/rt_b_wt" && echo "wt1" > wto.txt && git add wto.txt && git commit -q -m "wt_orig" \
        && git reset -q --hard HEAD^ && git reflog expire --expire=now --all )
    orig_oid_wt=$(git -C "$T/work/rt_b_wt" rev-parse -q --verify ORIG_HEAD)
    assert_ne "$orig_oid_wt" "" "reftable linked worktree should have ORIG_HEAD"

    run_borrow link --force "$T/work/rt_b"
    assert_exit0 "$BORROW_CODE" "linking reftable borrower to files hub should succeed ($BORROW_ERR)"
    run_borrow gc "$fhub" -- --prune=now
    assert_exit0 "$BORROW_CODE" "gc on files hub with reftable borrower should succeed ($BORROW_ERR)"
    assert_true_msg "reftable primary ORIG_HEAD commit pinned across hub gc" -- \
        bash -c 'git -C "$1" cat-file -e "$2" && git -C "$3" cat-file -e "$2"' _ "$fhub" "$orig_oid_primary" "$T/work/rt_b"
    assert_true_msg "reftable linked-worktree ORIG_HEAD commit pinned across hub gc" -- \
        bash -c 'git -C "$1" cat-file -e "$2" && git -C "$3" cat-file -e "$2"' _ "$fhub" "$orig_oid_wt" "$T/work/rt_b"
    assert_true_msg "reftable borrower fsck-clean after dedupe + gc" -- \
        bash -c 'git -C "$1" fsck --connectivity-only --no-progress >/dev/null 2>&1' _ "$T/work/rt_b"

    # (b) Reftable bare hub via init.defaultRefFormat=reftable + worktrees + mixed files borrower + submodule absorb.
    git config --global init.defaultRefFormat reftable
    local rthub="$T/hubs/example.test/org/sub.git"
    run_borrow clone -r https://example.test/org/sub.git "$T/work/rt_wt1"
    git config --global --unset init.defaultRefFormat
    assert_exit0 "$BORROW_CODE" "clone -r with init.defaultRefFormat=reftable should succeed ($BORROW_ERR)"
    assert_eq "$(git -C "$rthub" config --get extensions.refStorage)" "reftable" "bare hub should use reftable"
    assert_eq "$(git -C "$T/work/rt_wt1" config --get extensions.refStorage)" "reftable" "worktree inherits reftable"
    assert_eq "$(git -C "$T/work/rt_wt1" symbolic-ref --short HEAD)" "main" "reftable clone checks out main"
    assert_file_exists "$T/work/rt_wt1/nested/file.txt" "recursive submodule populated under reftable hub"

    run_borrow_in "$T/work/rt_wt1" add "$T/work/rt_wt2"
    assert_exit0 "$BORROW_CODE" "borrow add from reftable worktree should succeed ($BORROW_ERR)"
    assert_eq "$(git -C "$T/work/rt_wt2" symbolic-ref --short HEAD)" "rt_wt2" "second reftable worktree gets dir-name branch"

    # Link a classic `files` borrower against the `reftable` bare hub.
    git init -q --ref-format=files --initial-branch=main "$T/work/files_b"
    git -C "$T/work/files_b" remote add origin https://example.test/org/sub.git
    git -C "$T/work/files_b" fetch -q origin
    git -C "$T/work/files_b" reset -q --hard origin/main
    ( cd "$T/work/files_b" && git checkout -q --detach HEAD && echo "fb" > fb.txt \
        && git add fb.txt && git commit -q -m "files borrower commit" )
    local fb_oid
    fb_oid=$(git -C "$T/work/files_b" rev-parse HEAD)
    run_borrow link --force --hub "$rthub" "$T/work/files_b"
    assert_exit0 "$BORROW_CODE" "linking files borrower onto reftable hub should succeed ($BORROW_ERR)"
    run_borrow gc "$rthub" -- --prune=now
    assert_exit0 "$BORROW_CODE" "gc on reftable hub should succeed ($BORROW_ERR)"
    assert_true_msg "files borrower commit preserved by reftable hub gc" -- \
        bash -c 'git -C "$1" cat-file -e "$2" && git -C "$3" cat-file -e "$2"' _ "$rthub" "$fb_oid" "$T/work/files_b"

    # Lightweight absorb of a primary reftable standalone repo as a submodule.
    git init -q --ref-format=reftable --initial-branch=main "$T/work/rt_wt1/vendor/rt-sub"
    git -C "$T/work/rt_wt1/vendor/rt-sub" remote add origin https://example.test/org/subsub.git
    git -C "$T/work/rt_wt1/vendor/rt-sub" fetch -q origin
    git -C "$T/work/rt_wt1/vendor/rt-sub" reset -q --hard origin/main
    echo "rt-dirty" >> "$T/work/rt_wt1/vendor/rt-sub/file.txt"
    run_borrow_in "$T/work/rt_wt1" submodule vendor/rt-sub
    assert_exit0 "$BORROW_CODE" "absorbing reftable standalone submodule should succeed ($BORROW_ERR)"
    assert_true_msg "reftable submodule .git converted to gitdir link" -- test -f "$T/work/rt_wt1/vendor/rt-sub/.git"
    assert_eq "$(git -C "$T/work/rt_wt1/vendor/rt-sub" config --get extensions.refStorage)" "reftable" \
        "absorbed submodule preserves reftable format"
    assert_contains "$(cat "$(git -C "$T/work/rt_wt1/vendor/rt-sub" rev-parse --absolute-git-dir)/objects/info/alternates")" \
        "$fhub/objects" "absorbed reftable submodule borrows from hub"

    run_borrow status "$rthub"
    assert_exit0 "$BORROW_CODE" "status on reftable hub should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_OUT" "Role: hub" "status identifies reftable hub"
}
run_test reftable_hubs_and_borrowers test_reftable_hubs_and_borrowers

# ------------------------------------------------------------- summary ----

printf '\n==== %d/%d tests passed ====\n' "$PASS_COUNT" "$TEST_COUNT"
if [ "${#FAILURES[@]}" -gt 0 ]; then
    printf 'Failures:\n'
    printf '  %s\n' "${FAILURES[@]}"
    exit 1
fi
exit 0
