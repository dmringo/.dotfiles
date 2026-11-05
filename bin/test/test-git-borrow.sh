#!/usr/bin/env bash
# Test suite for git-borrow. Run: bash test-git-borrow.sh [name-substring ...]
# KEEP=1 bash test-git-borrow.sh   -> keep temp dirs instead of deleting them.
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
        *) fail "${msg:-assert_contains} (did not find [$needle])" ;;
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

assert_true() {
    "$@" || fail "assert_true failed: $*"
}

# assert_true_msg <msg> -- <cmd...>  (use when the command itself can't carry
# a trailing free-text message as an argument, e.g. `git cat-file -e <oid>`).
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
    [ "$code" -ne 0 ] || fail "${msg:-assert_exit_nonzero} (exit code was 0)"
}

# Runs git-borrow, capturing stdout/stderr/exit code into BORROW_OUT/BORROW_ERR/BORROW_CODE.
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

# Same as run_borrow but runs with cwd set to $1 first (git-borrow discovers
# the current repo from cwd for some commands, e.g. `worktree`).
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

# Builds (once) bare upstream repos under $BASE_FIXTURE/up:
#   subsub.git            (plain repo)
#   sub.git                (submodule "nested" -> subsub.git)
#   parent.git             (submodule "lib/sub" -> sub.git)
# Each commit carries a ~50000 byte random blob so dedupe savings are visible.
build_base_fixture() {
    [ -n "$BASE_FIXTURE" ] && return 0
    BASE_FIXTURE=$(mktemp -d /tmp/borrowtest-base.XXXX)
    command -v cygpath >/dev/null 2>&1 && BASE_FIXTURE=$(cygpath -m "$BASE_FIXTURE")
    ALL_TEMP_DIRS+=("$BASE_FIXTURE")
    local base=$BASE_FIXTURE
    mkdir -p "$base/up" "$base/src"

    # Isolate config while building the fixture too (never touch real ~/.gitconfig).
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

    if [ -n "$saved_global" ]; then
        export GIT_CONFIG_GLOBAL=$saved_global
    else
        unset GIT_CONFIG_GLOBAL
    fi
    if [ -n "$saved_nosystem" ]; then
        export GIT_CONFIG_NOSYSTEM=$saved_nosystem
    else
        unset GIT_CONFIG_NOSYSTEM
    fi
}

# Sets up a fresh isolated environment in $T with global config pointed only
# at $T, and a copy of the shared upstream fixture at $T/up.
# Exports: T, GIT_CONFIG_GLOBAL, GIT_CONFIG_NOSYSTEM.
new_env() {
    build_base_fixture
    T=$(mktemp -d /tmp/borrowtest.XXXX)
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
	insteadOf = git@mirror.test:other/
[uploadpack]
	allowFilter = true
	allowAnySHA1InWant = true
[gc]
	auto = 0
[maintenance]
	auto = false
[borrow]
	rootBase = $T/develop
[filter "lfs"]
	process = git-lfs filter-process
	required = true
EOF
    mkdir -p "$T/work"
}

# ------------------------------------------------------------- runner -----

should_run() {
    [ "$#" -eq 0 ] && return 0
    local name=$1
    [ "${#RUN_FILTERS[@]}" -eq 0 ] && return 0
    local f
    for f in "${RUN_FILTERS[@]}"; do
        case "$name" in
            *"$f"*) return 0 ;;
        esac
    done
    return 1
}

run_test() {
    [ "$#" -ge 2 ] || return 0
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

# (1) worktree -r auto-creates the root, recursively borrows a nested
# submodule, and status shows a borrower breadcrumb at every level.
test_worktree_root_autocreate() {
    new_env
    git clone -q https://example.test/org/parent.git "$T/src1"
    ( cd "$T/src1" && git submodule update -q --init --recursive )
    run_borrow_in "$T/src1" add -r -b topic "$T/work/wt1" main
    assert_exit0 "$BORROW_CODE" "worktree -r should succeed"

    # The top-level worktree shares src1's object store directly (that's how
    # `git worktree add` works) -- only the submodules are separate clones
    # that need their own roots.
    assert_file_exists "$T/develop/sub/.git" "root for sub (submodule) should be auto-created"
    assert_file_exists "$T/develop/subsub/.git" "root for subsub (nested submodule) should be auto-created"

    assert_file_exists "$T/work/wt1/lib/sub/nested/file.txt" "nested submodule should be checked out"

    run_borrow status "$T/work/wt1"
    assert_contains "$BORROW_OUT" "Role: none" "top worktree shares src1's store, so it is not itself a borrower"
    assert_contains "$BORROW_OUT" "lib/sub: borrower of $T/develop/sub" "status should show a breadcrumb for the submodule"
    run_borrow status "$T/work/wt1/lib/sub"
    assert_contains "$BORROW_OUT" "Role: borrower" "submodule lib/sub should be a borrower"
    run_borrow status "$T/work/wt1/lib/sub/nested"
    assert_contains "$BORROW_OUT" "Role: borrower" "nested submodule should be a borrower"
}
run_test worktree_root_autocreate test_worktree_root_autocreate

# (2) Recursion defaults: with submodule.recurse unset/false, plain
# `worktree` (no -r) leaves submodules uninitialized; --no-recursive
# overrides submodule.recurse=true; -r forces it on regardless.
test_recursion_defaults() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/src2"
    ( cd "$T/src2" && git submodule update -q --init )
    # Default (no submodule.recurse set): no -r means no submodule init.
    run_borrow_in "$T/src2" add "$T/work/wt2a" -b b2a --no-track
    assert_exit0 "$BORROW_CODE" "plain worktree add should succeed"
    assert_file_absent "$T/work/wt2a/nested/.git" "submodule should stay uninitialized without -r"

    # submodule.recurse=true in the source repo makes -r the default.
    git -C "$T/src2" config submodule.recurse true
    run_borrow_in "$T/src2" add "$T/work/wt2b" -b b2b --no-track
    assert_exit0 "$BORROW_CODE" "worktree add with submodule.recurse=true should succeed"
    assert_file_exists "$T/work/wt2b/nested/file.txt" "submodule should auto-init when submodule.recurse=true"

    # --no-recursive overrides submodule.recurse=true.
    run_borrow_in "$T/src2" add --no-recursive "$T/work/wt2c" -b b2c --no-track
    assert_exit0 "$BORROW_CODE" "--no-recursive should succeed"
    assert_file_absent "$T/work/wt2c/nested/.git" "submodule should stay uninitialized with --no-recursive"

    # -r forces recursion even with submodule.recurse unset/false.
    git -C "$T/src2" config submodule.recurse false
    run_borrow_in "$T/src2" add -r "$T/work/wt2d" -b b2d --no-track
    assert_exit0 "$BORROW_CODE" "-r should succeed"
    assert_file_exists "$T/work/wt2d/nested/file.txt" "-r should force submodule init"
}
run_test recursion_defaults test_recursion_defaults

# (3) link on a fat standalone clone: old alternates + loose objects + a
# multi-pack-index dedupe to ~0, the repo stays fsck-clean, no repack/fsck
# subprocess runs, and re-running link is a no-op (idempotent).
test_link_fat_clone() {
    new_env
    # A "fat" clone: full history clone, then make it fat via an alternates
    # detour (clone --reference) so it has loose objects + an old alternates
    # file, then repack into a multi-pack-index.
    git clone -q https://example.test/org/sub.git "$T/src3"
    git clone -q --reference "$T/src3" --dissociate https://example.test/org/sub.git "$T/work/wt3"
    # Recreate the "old alternates" situation `dissociate` just removed: point
    # wt3 at src3's objects again, with some loose objects of its own too.
    echo "$T/src3/.git/objects" > "$T/work/wt3/.git/objects/info/alternates"
    git -C "$T/work/wt3" repack -a -d -q
    git -C "$T/work/wt3" multi-pack-index write --bitmap 2>/dev/null || git -C "$T/work/wt3" multi-pack-index write
    assert_file_exists "$T/work/wt3/.git/objects/pack/multi-pack-index" "setup: multi-pack-index should exist before link"
    assert_file_exists "$T/work/wt3/.git/objects/info/alternates" "setup: old alternates should exist before link"

    # Pre-map the URL to the intended root path so link doesn't hit the
    # name-mismatch prompt (that interactive path is scenario 12's concern).
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub"

    local before_kib
    before_kib=$(git --git-dir="$T/work/wt3/.git" count-objects -v | awk '/^size: |^size-pack: /{s+=$2} END{print s+0}')

    run_borrow link "$T/work/wt3"
    assert_exit0 "$BORROW_CODE" "link should succeed on a fat clone ($BORROW_ERR)"

    local after_kib
    after_kib=$(git --git-dir="$T/work/wt3/.git" count-objects -v | awk '/^size: |^size-pack: /{s+=$2} END{print s+0}')
    [ "$after_kib" -lt $((before_kib / 4)) ] ||
        fail "dedupe should shrink local objects a lot (before=${before_kib}KiB after=${after_kib}KiB)"
    assert_file_absent "$T/work/wt3/.git/objects/pack/multi-pack-index" "stale multi-pack-index should be removed by dedupe"

    local alt
    alt=$(cat "$T/work/wt3/.git/objects/info/alternates")
    assert_eq "$alt" "$T/develop/sub/.git/objects" "alternates should point only at the root after dedupe"

    # Idempotent re-link: should not error, and should not shrink further.
    run_borrow link "$T/work/wt3"
    assert_exit0 "$BORROW_CODE" "re-linking an already-linked clone should succeed ($BORROW_ERR)"
    local reafter_kib
    reafter_kib=$(git --git-dir="$T/work/wt3/.git" count-objects -v | awk '/^size: |^size-pack: /{s+=$2} END{print s+0}')
    assert_eq "$reafter_kib" "$after_kib" "re-link should be idempotent (no further change in local object size)"

    assert_true_msg "wt3 should pass our own fsck after dedupe" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$T/work/wt3"

    # A plain re-link WITHOUT --verify should be walk-free: it must not spawn
    # a child "repack" or "fsck" process (fetch/rev-list/pack-objects/cat-file
    # are all legitimate and expected).
    local trace_file
    trace_file=$(mktemp)
    GIT_TRACE2_EVENT="$trace_file" run_borrow link "$T/work/wt3"
    assert_exit0 "$BORROW_CODE" "re-link without --verify should succeed ($BORROW_ERR)"
    local bad_children
    bad_children=$(grep -o '"argv":\[[^]]*\]' "$trace_file" | grep -E '"(repack|fsck)"' || true)
    assert_eq "$bad_children" "" "a walk-free re-link should not spawn a repack or fsck child process"
    rm -f "$trace_file"
}
run_test link_fat_clone test_link_fat_clone

# (4) Objects unique to the reflog or the staged index (not reachable from
# any ref) must stay local after dedupe, even though the root doesn't have
# them -- dedupe only removes objects the root can supply.
test_dedupe_keeps_reflog_and_index_objects() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt4"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"

    # An object reachable only from a reflog entry (amend the commit away).
    ( cd "$T/work/wt4" && echo reflog-only > reflog-only.txt && git add reflog-only.txt \
        && git commit -q -m "will be amended away" \
        && git commit -q --amend -m "amended, orphaning the previous commit" )
    local reflog_oid
    reflog_oid=$(git -C "$T/work/wt4" log -g --format=%H | sed -n 2p)

    # An object that exists only in the staged index (never committed).
    echo staged-only > "$T/work/wt4/staged-only.txt"
    git -C "$T/work/wt4" add staged-only.txt
    local staged_oid
    staged_oid=$(git -C "$T/work/wt4" rev-parse :staged-only.txt)

    run_borrow link "$T/work/wt4"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"

    assert_true_msg "reflog-only commit object should still be present locally" -- \
        git -C "$T/work/wt4" cat-file -e "$reflog_oid"
    assert_true_msg "staged-only blob should still be present locally" -- \
        git -C "$T/work/wt4" cat-file -e "$staged_oid"
}
run_test dedupe_keeps_reflog_and_index test_dedupe_keeps_reflog_and_index_objects

# (5) An object reachable only via a reflog entry, that exists ONLY in an old
# alternate (not in the root and not local), forces that old alternate to be
# kept as a secondary alternate. After `reflog expire --expire=now --all`
# removes the dangling reflog entry, re-linking drops the old alternate.
test_old_alternate_kept_then_dropped() {
    new_env
    # Build a second "donor" repo with a commit the root will never have,
    # and graft an old-alternates relationship onto wt5 pointing at it, with
    # a reflog entry as the only reachability path to that commit.
    git clone -q https://example.test/org/subsub.git "$T/donor"
    ( cd "$T/donor" && echo donor-only > donor.txt && git add -A && git commit -q -m "donor-only commit" )
    local donor_oid
    donor_oid=$(git -C "$T/donor" rev-parse HEAD)

    git clone -q --reference "$T/donor" --dissociate https://example.test/org/subsub.git "$T/work/wt5"
    # Point wt5 at donor as an "old alternate" and fabricate a reflog entry
    # that is the only reason donor_oid is reachable (no local ref points at
    # it, and it is not in wt5's own object store since --dissociate already
    # ran before we add this old alternate back).
    echo "$T/donor/.git/objects" > "$T/work/wt5/.git/objects/info/alternates"
    mkdir -p "$T/work/wt5/.git/logs"
    printf '%s %s T <t@e> 0 +0000\tcommit: donor-only\n' "$(git -C "$T/work/wt5" rev-parse HEAD)" "$donor_oid" \
        >> "$T/work/wt5/.git/logs/HEAD"

    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"

    run_borrow link "$T/work/wt5"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "keeping them as secondary alternates" \
        "link should warn that the old alternate is kept (reflog-only object)"
    assert_contains "$(cat "$T/work/wt5/.git/objects/info/alternates")" "$T/donor/.git/objects" \
        "old alternate should still be listed after link"
    assert_true_msg "donor-only commit should still be reachable" -- \
        git -C "$T/work/wt5" cat-file -e "$donor_oid"

    # Expire the reflog entry: the commit is no longer reachable at all, so
    # the next link should drop the now-unneeded old alternate.
    git -C "$T/work/wt5" reflog expire --expire=now --all
    run_borrow link "$T/work/wt5"
    assert_exit0 "$BORROW_CODE" "re-link after reflog expire should succeed ($BORROW_ERR)"
    assert_not_contains "$(cat "$T/work/wt5/.git/objects/info/alternates" 2>/dev/null)" "$T/donor" \
        "old alternate should be dropped once nothing needs it"
}
run_test old_alternate_kept_then_dropped test_old_alternate_kept_then_dropped

# (6) SIGTERM delivered mid-`link` (during the reachability check, which is
# the long-running step before alternates are committed) must leave the
# repository exactly as it was: alternates restored, no leftover lock/tmp
# files.
test_sigterm_restores_alternates() {
    new_env
    # Needs an "old alternates" situation (like scenario 3/5) so link's slow
    # reachability check (rev-list --alternate-refs) actually runs; without
    # an old alternate to reconcile, that check is skipped entirely.
    git clone -q https://example.test/org/subsub.git "$T/src6"
    git clone -q --reference "$T/src6" --dissociate https://example.test/org/subsub.git "$T/work/wt6"
    echo "$T/src6/.git/objects" > "$T/work/wt6/.git/objects/info/alternates"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"

    local orig_alt=
    [ -f "$T/work/wt6/.git/objects/info/alternates" ] && orig_alt=$(cat "$T/work/wt6/.git/objects/info/alternates")

    # A `git` shim on PATH that stalls exactly one `rev-list --alternate-refs`
    # invocation (the reachability check) long enough for us to SIGTERM the
    # whole git-borrow process group, then execs the real git for everything
    # else so link can otherwise proceed normally up to that point.
    local shimdir real_git
    shimdir=$(mktemp -d)
    ALL_TEMP_DIRS+=("$shimdir")
    real_git=$(command -v git)
    cat > "$shimdir/git" <<EOF
#!/usr/bin/env bash
case " \$* " in
    *" --alternate-refs "*)
        touch "$T/work/.rev-list-started"
        sleep 20 &
        wait \$!
        exit 143
        ;;
esac
exec "$real_git" "\$@"
EOF
    chmod +x "$shimdir/git"

    PATH="$shimdir:$PATH" git-borrow link "$T/work/wt6" >"$T/borrow.out" 2>"$T/borrow.err" &
    local borrow_pid=$!
    local waited=0
    while [ ! -e "$T/work/.rev-list-started" ] && [ "$waited" -lt 100 ]; do
        sleep 0.1
        waited=$((waited + 1))
    done
    assert_file_exists "$T/work/.rev-list-started" "setup: the shimmed rev-list should have started"
    kill -INT "$borrow_pid" 2>/dev/null || kill -TERM "$borrow_pid" 2>/dev/null
    wait "$borrow_pid" 2>/dev/null
    local code=$?

    assert_ne "$code" 0 "interrupted link should not report success"
    assert_file_absent "$T/work/wt6/.git/borrow-dedupe.lock" "dedupe lock file should not be left behind"
    local after_alt=
    [ -f "$T/work/wt6/.git/objects/info/alternates" ] && after_alt=$(cat "$T/work/wt6/.git/objects/info/alternates")
    assert_eq "$after_alt" "$orig_alt" "alternates should be restored to their pre-link state after SIGTERM"
    assert_true_msg "wt6 should still pass fsck after the interrupted link" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$T/work/wt6"
}
run_test sigterm_restores_alternates test_sigterm_restores_alternates

# (7) Partial clone combinations: (a) a partial borrower + a full root works
# (promisor pack skipped only if the root itself is partial -- here it isn't,
# so the borrower's promisor objects the root already has get deduped), and
# (b) a partial borrower + a partial root keeps promisor packs untouched.
test_partial_clone_combinations() {
    new_env
    # Push a second commit upstream that REPLACES blob.bin's content (rather
    # than just adding a file), so the old blob.bin is reachable only via
    # history, not the tip tree -- a --filter=blob:none clone's checkout then
    # brings down only the NEW blob.bin, leaving the old one genuinely
    # missing locally until something fetches it.
    git clone -q https://example.test/org/subsub.git "$T/pusher"
    local old_blob_oid
    old_blob_oid=$(git -C "$T/pusher" rev-parse HEAD:blob.bin)
    ( cd "$T/pusher" && head -c 9000 /dev/urandom > extra.bin && head -c 50000 /dev/urandom > blob.bin \
        && git add -A && git commit -q -m "extra commit (replaces blob.bin)" && git push -q origin HEAD:main )

    # (a) partial borrower + full root: full root already has everything
    # (including the old, history-only blob), so the borrower's promisor
    # pack becomes fully redundant and is removed entirely. This is also a
    # regression test for a bug where create_root could seed a root from a
    # partial clone (copying its promisor packs but not its promisor config)
    # and leave the root itself broken/partial; the root created here must
    # come out NOT partial.
    git clone -q --filter=blob:none https://example.test/org/subsub.git "$T/work/wt7a"
    assert_eq "$(git -C "$T/work/wt7a" config --get remote.origin.promisor)" "true" \
        "setup: wt7a should be a partial clone"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub-a"
    run_borrow link "$T/work/wt7a"
    assert_exit0 "$BORROW_CODE" "link of partial borrower + full root should succeed ($BORROW_ERR)"
    assert_true_msg "wt7a should pass fsck after link" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$T/work/wt7a"
    assert_true_msg "log should still work in wt7a after link" -- \
        bash -c 'git -C "$1" log --oneline -q >/dev/null' _ "$T/work/wt7a"

    local root_a="$T/develop/subsub-a"
    assert_eq "$(find "$T/work/wt7a/.git/objects/pack" -maxdepth 1 -name '*.promisor' | wc -l)" "0" \
        "wt7a's promisor packs should be fully gone once the root supplied everything"
    assert_true_msg "full history (including the replaced blob) should walk locally without lazy fetch" -- \
        bash -c 'env GIT_NO_LAZY_FETCH=1 git -C "$1" log -p --oneline -q >/dev/null' _ "$T/work/wt7a"
    assert_eq "$(git -C "$root_a" config --get remote.origin.promisor 2>/dev/null)" "" \
        "the root created to seed wt7a must not itself be a partial clone (no promisor remote)"
    assert_eq "$(git -C "$root_a" config --get extensions.partialClone 2>/dev/null)" "" \
        "the root should not have extensions.partialClone set"
    assert_eq "$(find "$root_a/.git/objects/pack" -maxdepth 1 -name '*.promisor' | wc -l)" "0" \
        "the root should have no .promisor pack files"
    assert_true_msg "the root should have the old (pre-replacement) blob" -- \
        git -C "$root_a" cat-file -e "$old_blob_oid"

    # (c) residual-promisor case: partial borrower + full root, where the
    # borrower has a branch (and objects) the root doesn't have. After
    # link/dedupe, the root can't supply those objects, so they must be kept
    # locally in a new residual pack -- still marked as promisor (dedupe
    # preserves "promisor-ness" of what it can't hand off to the root) -- and
    # must be readable without any further lazy fetch.
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub-c"
    git clone -q https://example.test/org/subsub.git "$T/work/wt7c-seed"
    run_borrow root "$T/work/wt7c-seed"
    assert_exit0 "$BORROW_CODE" "setup: seeding root c should succeed ($BORROW_ERR)"
    local root_c="$T/develop/subsub-c"

    # Push a branch upstream only AFTER the root exists, so the root never
    # learns about it.
    git clone -q https://example.test/org/subsub.git "$T/pusher2"
    ( cd "$T/pusher2" && git checkout -q -b side-branch && head -c 7000 /dev/urandom > side.bin \
        && git add -A && git commit -q -m "side branch commit" && git push -q origin HEAD:side-branch )
    assert_true_msg "setup: root c should not know about side-branch" -- \
        bash -c '! git -C "$1" rev-parse -q --verify refs/remotes/origin/side-branch >/dev/null' _ "$root_c"

    git clone -q --filter=blob:none https://example.test/org/subsub.git "$T/work/wt7c"
    git -C "$T/work/wt7c" fetch -q origin side-branch
    local side_oid side_blob_oid
    side_oid=$(git -C "$T/work/wt7c" rev-parse FETCH_HEAD)
    side_blob_oid=$(git -C "$T/work/wt7c" rev-parse "FETCH_HEAD:side.bin")
    # Materialize the (lazily-fetchable) blob locally, then drop the only ref
    # that pointed at any of this -- dedupe inventories local pack contents
    # directly, not ref reachability, so these objects must still be found.
    git -C "$T/work/wt7c" cat-file -p "$side_blob_oid" >/dev/null
    git -C "$T/work/wt7c" update-ref -d refs/remotes/origin/side-branch 2>/dev/null || true

    run_borrow link "$T/work/wt7c"
    assert_exit0 "$BORROW_CODE" "link of partial borrower with root-missing branch objects should succeed ($BORROW_ERR)"

    local promisor_pack
    promisor_pack=$(find "$T/work/wt7c/.git/objects/pack" -maxdepth 1 -name '*.promisor' | head -n 1)
    assert_true_msg "wt7c should still have a residual promisor pack after dedupe" -- test -n "$promisor_pack"
    assert_true_msg "the root-missing commit should be readable locally without lazy fetch" -- \
        env GIT_NO_LAZY_FETCH=1 git -C "$T/work/wt7c" cat-file -e "$side_oid"
    assert_true_msg "the root-missing blob should be readable locally without lazy fetch" -- \
        env GIT_NO_LAZY_FETCH=1 git -C "$T/work/wt7c" cat-file -e "$side_blob_oid"
    assert_true_msg "wt7c should pass fsck after link" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$T/work/wt7c"

    # (b) partial borrower + partial root: create the root itself as a
    # partial clone first (via --seed-from a partial clone with no full
    # local copy available), then link another partial clone to it; its
    # promisor packs must be left untouched (root can't supply blobs it
    # doesn't have either).
    git clone -q --filter=blob:none https://example.test/org/subsub.git "$T/develop/subsub-b"
    ( cd "$T/develop/subsub-b" && git config borrow.isRoot true )
    assert_eq "$(git -C "$T/develop/subsub-b" config --get remote.origin.promisor)" "true" \
        "setup: subsub-b root should be a partial clone"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub-b"

    git clone -q --filter=blob:none https://example.test/org/subsub.git "$T/work/wt7b"
    run_borrow link "$T/work/wt7b"
    assert_exit0 "$BORROW_CODE" "link of partial borrower + partial root should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "promisor pack, and the root is a partial clone" \
        "dedupe should report skipping the promisor pack because the root is partial"
    assert_true_msg "wt7b should pass fsck after link" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$T/work/wt7b"
}
run_test partial_clone_combinations test_partial_clone_combinations

# (8) .keep pack handling: a pinned pack (no fetch-pack/receive-pack marker)
# is left alone with a "pinned" reason; a pack with a stale fetch-pack .keep
# (pid long gone) is reported STALE and left alone; a pack whose .keep
# belongs to a still-running process is reported "in use" and left alone.
test_keep_pack_handling() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt8"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"

    git -C "$T/work/wt8" repack -a -d -q
    local pack
    pack=$(ls "$T/work/wt8/.git/objects/pack"/pack-*.pack | head -n 1)
    local base=${pack%.pack}

    # Pinned pack: an empty .keep (no recognizable fetch-pack/receive-pack line).
    : > "$base.keep"

    run_borrow link "$T/work/wt8"
    assert_exit0 "$BORROW_CODE" "link should succeed with a pinned pack ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "pinned" "pinned .keep pack should be reported as pinned"
    assert_file_exists "$base.pack" "pinned pack should not be removed"

    # Stale .keep: looks like a fetch-pack left it behind, but that pid is gone.
    local dead_pid
    dead_pid=$(( $(cat /proc/sys/kernel/pid_max 2>/dev/null || echo 99999) - 2 ))
    printf 'fetch-pack %s on %s\n' "$dead_pid" "$(hostname)" > "$base.keep"
    run_borrow link "$T/work/wt8"
    assert_exit0 "$BORROW_CODE" "link should succeed with a stale .keep ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "STALE" "stale .keep pack should be reported as STALE"
    assert_file_exists "$base.pack" "stale-keep pack should not be removed automatically"

    # Live .keep: pid of a real, currently-running process (this shell).
    printf 'fetch-pack %s on %s\n' "$$" "$(hostname)" > "$base.keep"
    run_borrow link "$T/work/wt8"
    assert_exit0 "$BORROW_CODE" "link should succeed with a live .keep ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "in use by running fetch-pack" "live .keep pack should be reported as in use"
    assert_file_exists "$base.pack" "live-keep pack should not be removed"

    rm -f "$base.keep"
}
run_test keep_pack_handling test_keep_pack_handling

breadcrumb_borrower_id() {
    sed -n 's/^id=//p' "$(git -C "$1" rev-parse --path-format=absolute --git-common-dir)/borrow-origin" | tail -n 1
}

# (9) Root gc safety: `git borrow gc` syncs registered borrowers' worktree
# HEADs into the root (as refs/borrowers/<id>/heads/*) before pruning, so a
# commit a borrower has checked out detached -- with no other ref, anywhere,
# keeping it reachable -- survives even `git gc -- --prune=now`. A negative
# control proves it's genuinely that mechanism doing the saving: remove a
# borrower's registered refs by hand and gc with plain `git gc` (which also
# bypasses the pre-auto-gc hook, since that only fires for *automatic* gc)
# and the same kind of commit is lost. The installed pre-auto-gc hook runs
# sync too (and genuinely advances remote.borrower/<id>.lastSync), while
# refusing to run (exit 1, skipping auto gc) while GIT_BORROW_SYNCING is set,
# so a sync-triggered auto-gc can't recurse into itself.
test_root_gc_safety() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt9"
    git -C "$T/work/wt9" config core.worktree "$T/work/wt9"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"
    run_borrow link "$T/work/wt9"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"

    local root="$T/develop/subsub"
    assert_file_exists "$root/.git/hooks/pre-auto-gc" "link should install a pre-auto-gc hook in the root"
    assert_true_msg "pre-auto-gc hook should be executable" -- test -x "$root/.git/hooks/pre-auto-gc"

    # --- Protected case -----------------------------------------------
    # Commit X exists in the root only on a temp branch; delete that branch
    # and expire the root's reflog, so nothing in the ROOT keeps X reachable
    # any more -- only wt9's checked-out (detached) HEAD does, once synced.
    git -C "$root" checkout -q -b tmpX
    echo dataX > "$root/fileX.txt"
    git -C "$root" add -A
    git -C "$root" commit -q -m "commit X"
    local X
    X=$(git -C "$root" rev-parse HEAD)
    git -C "$root" checkout -q main

    git -C "$T/work/wt9" fetch -q root
    git -C "$T/work/wt9" checkout -q --detach "$X"

    git -C "$root" branch -D tmpX >/dev/null
    git -C "$root" reflog expire --expire=now --all

    local id1
    id1=$(breadcrumb_borrower_id "$T/work/wt9")

    run_borrow gc "$root" -- --prune=now
    assert_exit0 "$BORROW_CODE" "git borrow gc should succeed ($BORROW_ERR)"
    assert_true_msg "X should survive root gc (kept reachable via sync of wt9's detached HEAD)" -- \
        git -C "$root" cat-file -e "$X"
    assert_true_msg "X should be readable from the borrower too" -- \
        git -C "$T/work/wt9" cat-file -e "$X"
    assert_true_msg "wt9 should stay fsck-clean" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$T/work/wt9"

    # --- Hook behavior --------------------------------------------------
    local old_lastsync
    old_lastsync=$(git -C "$root" config --get "remote.borrower/$id1.lastSync")

    # The hook must refuse to run (and thus skip auto gc) while a sync is in
    # flight, so a sync-triggered auto-gc cannot recursively re-enter itself.
    # Real git invokes hooks with cwd inside the repo, so `cd` there first.
    ( cd "$root" && GIT_BORROW_SYNCING=1 ./.git/hooks/pre-auto-gc )
    local hook_code=$?
    assert_ne "$hook_code" 0 "pre-auto-gc hook should exit non-zero (skip gc) when GIT_BORROW_SYNCING is set"

    # Without GIT_BORROW_SYNCING, the hook should run sync successfully, and
    # that sync should genuinely advance lastSync (not just report success).
    sleep 1
    ( cd "$root" && ./.git/hooks/pre-auto-gc )
    assert_eq "$?" 0 "pre-auto-gc hook should succeed (run sync) when GIT_BORROW_SYNCING is unset"
    local new_lastsync
    new_lastsync=$(git -C "$root" config --get "remote.borrower/$id1.lastSync")
    assert_ne "$new_lastsync" "$old_lastsync" "remote.borrower/<id>.lastSync should actually change when the hook runs sync"

    # --- Negative control ------------------------------------------------
    # Same setup with a second borrower, but its refs/borrowers/<id>/
    # namespace is erased by hand (as if it had never been synced), and gc
    # is run directly via plain `git gc` -- bypassing both `git borrow gc`'s
    # sync step and the pre-auto-gc hook (which never fires for a manual,
    # non-"--auto" gc). Nothing protects commit Y, so it must be pruned,
    # proving the registered-borrower-refs mechanism above is what actually
    # saved X.
    git clone -q https://example.test/org/subsub.git "$T/work/wt9b"
    git -C "$T/work/wt9b" config core.worktree "$T/work/wt9b"
    run_borrow link "$T/work/wt9b"
    assert_exit0 "$BORROW_CODE" "link of second borrower should succeed ($BORROW_ERR)"

    git -C "$root" checkout -q -b tmpY
    echo dataY > "$root/fileY.txt"
    git -C "$root" add -A
    git -C "$root" commit -q -m "commit Y"
    local Y
    Y=$(git -C "$root" rev-parse HEAD)
    git -C "$root" checkout -q main

    git -C "$T/work/wt9b" fetch -q root
    git -C "$T/work/wt9b" checkout -q --detach "$Y"

    git -C "$root" branch -D tmpY >/dev/null
    git -C "$root" reflog expire --expire=now --all

    local id2
    id2=$(breadcrumb_borrower_id "$T/work/wt9b")
    git -C "$root" for-each-ref --format='delete %(refname)' "refs/borrowers/$id2/" |
        git -C "$root" update-ref --stdin

    git -C "$root" gc --prune=now >/dev/null 2>&1

    assert_true_msg "without protection, Y should be pruned from the root" -- \
        bash -c '! git -C "$1" cat-file -e "$2" 2>/dev/null' _ "$root" "$Y"
    assert_true_msg "and thus Y should be missing from the unprotected borrower too" -- \
        bash -c '! git -C "$1" cat-file -e "$2" 2>/dev/null' _ "$T/work/wt9b" "$Y"
}
run_test root_gc_safety test_root_gc_safety

# (9b) `cmd_sync` syncs every borrower independently (each in its own
# set-e subshell) and keeps going past a failure, warning which borrower
# failed and why the root is still safe; but `sync`/`gc` as a whole must
# still fail if *any* borrower couldn't be synced, since gc could then prune
# objects only that borrower needs. Give one of two borrowers of the same
# root a ref to a nonexistent object (simulating corruption/partial state)
# so syncing it fails; confirm the healthy borrower still gets synced
# (lastSync advances) while the broken one doesn't, sync/gc both fail loudly
# and gc never actually runs, and the hook (which calls sync) fails too.
# Forgetting the broken borrower restores normal operation.
test_sync_continues_past_broken_borrower() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt_good"
    git -C "$T/work/wt_good" config core.worktree "$T/work/wt_good"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"
    run_borrow link "$T/work/wt_good"
    assert_exit0 "$BORROW_CODE" "link of good borrower should succeed ($BORROW_ERR)"

    git clone -q https://example.test/org/subsub.git "$T/work/wt_bad"
    git -C "$T/work/wt_bad" config core.worktree "$T/work/wt_bad"
    run_borrow link "$T/work/wt_bad"
    assert_exit0 "$BORROW_CODE" "link of (soon to be broken) borrower should succeed ($BORROW_ERR)"

    local root="$T/develop/subsub"
    local id_good id_bad
    id_good=$(breadcrumb_borrower_id "$T/work/wt_good")
    id_bad=$(breadcrumb_borrower_id "$T/work/wt_bad")
    local lastsync_good_before lastsync_bad_before
    lastsync_good_before=$(git -C "$root" config --get "remote.borrower/$id_good.lastSync")
    lastsync_bad_before=$(git -C "$root" config --get "remote.borrower/$id_bad.lastSync")

    # Break wt_bad: give it a ref pointing at an object that doesn't exist
    # anywhere, so the root's fetch-from-borrower step in sync_borrower fails.
    echo 0123456789012345678901234567890123456789 > "$T/work/wt_bad/.git/refs/heads/broken"

    sleep 1
    run_borrow sync "$root"
    assert_exit_nonzero "$BORROW_CODE" "sync should fail overall when one borrower is broken"
    assert_contains "$BORROW_ERR" "sync failed for borrower $id_bad" \
        "sync's warning should name the broken borrower's id"
    assert_contains "$BORROW_ERR" "$T/work/wt_bad" \
        "sync's warning should name the broken borrower's worktree"

    local lastsync_good_after lastsync_bad_after
    lastsync_good_after=$(git -C "$root" config --get "remote.borrower/$id_good.lastSync")
    lastsync_bad_after=$(git -C "$root" config --get "remote.borrower/$id_bad.lastSync")
    assert_ne "$lastsync_good_after" "$lastsync_good_before" \
        "the healthy borrower should still get synced (lastSync advances) despite the other one failing"
    assert_eq "$lastsync_bad_after" "$lastsync_bad_before" \
        "the broken borrower's lastSync should NOT advance (its sync failed before reaching that point)"

    # Plant an unreachable loose object with an old mtime in the root: if gc
    # actually ran with --prune=now it would be gone immediately regardless
    # of age, so its survival proves gc itself never ran.
    local unreachable_oid unreachable_path
    unreachable_oid=$(echo "unreachable object for gc-skip test" | git -C "$root" hash-object -w --stdin)
    unreachable_path="$root/.git/objects/${unreachable_oid:0:2}/${unreachable_oid:2}"
    assert_file_exists "$unreachable_path" "setup: unreachable loose object should exist"
    touch -d '1 year ago' "$unreachable_path"

    run_borrow gc "$root" -- --prune=now
    assert_exit_nonzero "$BORROW_CODE" "git borrow gc should refuse to run while a borrower can't be synced"
    assert_contains "$BORROW_ERR" "not running gc" "gc should explain it refused to run"
    assert_file_exists "$unreachable_path" "gc must never have actually run; the unreachable object should still be there"

    ( cd "$root" && ./.git/hooks/pre-auto-gc ) >/dev/null 2>&1
    local hook_code=$?
    assert_ne "$hook_code" 0 "pre-auto-gc hook should fail too, since it just runs sync"

    # Forgetting the broken borrower restores normal operation.
    run_borrow forget --force --root "$root" "$id_bad"
    assert_exit0 "$BORROW_CODE" "forget --force should succeed ($BORROW_ERR)"

    run_borrow sync "$root"
    assert_exit0 "$BORROW_CODE" "sync should succeed again once the broken borrower is forgotten ($BORROW_ERR)"

    run_borrow gc "$root" -- --prune=now
    assert_exit0 "$BORROW_CODE" "gc should succeed again once the broken borrower is forgotten ($BORROW_ERR)"
    assert_file_absent "$unreachable_path" \
        "gc should have actually run this time, pruning the now-unprotected unreachable object"
}
run_test sync_continues_past_broken_borrower test_sync_continues_past_broken_borrower

# (10) A borrower's own remote-tracking refs are not mirrored into the root --
# sync only pushes the borrower's worktree HEAD(s) as anonymous refs under
# refs/borrowers/<id>/heads/*, via a refspec with a negative `^refs/remotes/*`
# clause. Push a brand-new branch upstream (with a new blob the root has
# never seen), let the borrower fetch it into refs/remotes/origin/extra, and
# re-link: the root must NOT gain that commit (nor any
# refs/borrowers/<id>/refs/remotes/* ref), while the borrower keeps the
# commit/blob available locally (not via alternates/lazy-fetch, since
# dedupe_objects only strips objects the root already has). The root stays
# fsck-clean throughout, including after a prune.
test_borrower_remotes_not_mirrored() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt10"
    git -C "$T/work/wt10" config core.worktree "$T/work/wt10"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"
    run_borrow link "$T/work/wt10"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"
    local root="$T/develop/subsub"

    # Push a new branch upstream, with a new blob, that the root has never
    # fetched -- then let the borrower (only) pick it up as a remote-tracking
    # ref.
    git clone -q https://example.test/org/subsub.git "$T/pusher10"
    ( cd "$T/pusher10" && git checkout -q -b extra \
        && head -c 7000 /dev/urandom > extra-blob.bin && git add -A \
        && git commit -q -m "extra branch commit" && git push -q origin extra:extra )
    local extra_oid
    extra_oid=$(git -C "$T/pusher10" rev-parse extra)

    git -C "$T/work/wt10" fetch -q origin extra:refs/remotes/origin/extra
    assert_true_msg "setup: wt10 should have a refs/remotes/origin/extra ref" -- \
        bash -c 'git -C "$1" rev-parse -q --verify refs/remotes/origin/extra >/dev/null' _ "$T/work/wt10"

    run_borrow link "$T/work/wt10"
    assert_exit0 "$BORROW_CODE" "re-link should succeed ($BORROW_ERR)"

    assert_true_msg "root should NOT gain the borrower's remote-tracking commit" -- \
        bash -c '! git -C "$1" cat-file -e "$2" 2>/dev/null' _ "$root" "$extra_oid"

    local id
    id=$(breadcrumb_borrower_id "$T/work/wt10")
    local mirrored
    mirrored=$(git -C "$root" for-each-ref "refs/borrowers/$id/refs/remotes/" | wc -l)
    assert_eq "$mirrored" "0" "root should have no refs/borrowers/<id>/refs/remotes/* refs from the borrower's remote-tracking refs"

    assert_true_msg "the borrower should still have the commit locally, without lazy-fetching" -- \
        bash -c 'GIT_NO_LAZY_FETCH=1 git -C "$1" cat-file -e "$2" 2>/dev/null' _ "$T/work/wt10" "$extra_oid"

    assert_true_msg "root should stay fsck-clean" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$root"

    git -C "$root" gc --prune=now >/dev/null 2>&1
    assert_true_msg "borrower should stay fsck-clean after a root prune" -- \
        bash -c 'git -C "$1" fsck --connectivity-only >/dev/null 2>&1' _ "$T/work/wt10"
}
run_test borrower_remotes_not_mirrored test_borrower_remotes_not_mirrored

# (11a) `link --worktreeify` on a plain standalone clone: dirty tracked
# changes and an untracked file survive the conversion to a linked worktree;
# a branch whose upstream remote has a different name in the clone than in
# the root gets remapped via remote_for_url when copied in; an existing
# stash is archived (warned about, kept as a ref in the root) rather than
# lost.
test_worktreeify_basic() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wtA"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"
    local root="$T/develop/subsub"
    # Seed+create the root first via a normal (non-worktreeify) link, so it
    # already has "main" at the same commit -- any genuinely new branch
    # created afterwards exercises the branch-copy/remap path for real.
    run_borrow root "$T/work/wtA"
    assert_exit0 "$BORROW_CODE" "root creation should succeed ($BORROW_ERR)"

    ( cd "$T/work/wtA" && \
        git remote add upstream https://example.test/org/subsub.git && \
        git checkout -q -b feature && \
        git config branch.feature.remote upstream && \
        git config branch.feature.merge refs/heads/feature && \
        echo stashme > stash.txt && git add -A && \
        git stash push -q -u -m "wip stash" )

    ( cd "$T/work/wtA" && echo modified >> file.txt && echo untracked > extra.txt )
    local status_before
    status_before=$(git -C "$T/work/wtA" status --porcelain | sort)

    run_borrow link "$T/work/wtA"
    assert_exit0 "$BORROW_CODE" "worktreeify should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "stash is kept as" "should warn that the old stash is archived"

    assert_true_msg "wtA/.git should now be a worktree gitdir-link file" -- \
        grep -q '^gitdir:' "$T/work/wtA/.git"
    assert_file_exists "$T/work/wtA/.git.pre-worktreeify" "old git dir should be preserved for review"

    local status_after
    status_after=$(git -C "$T/work/wtA" status --porcelain | sort)
    assert_eq "$status_after" "$status_before" "dirty+untracked state should be unchanged by worktreeify"

    assert_true_msg "root should have gained the new 'feature' branch" -- \
        bash -c 'git -C "$1" rev-parse -q --verify refs/heads/feature >/dev/null' _ "$root"
    local remap_remote
    remap_remote=$(git -C "$root" config --get branch.feature.remote)
    assert_eq "$remap_remote" "origin" "branch.feature.remote should be remapped from 'upstream' (wt-side name) to the root's 'origin'"

    local stash_refs
    stash_refs=$(git -C "$root" for-each-ref --format='%(refname)' refs/borrowers/ | grep -c '/refs/stash$' || true)
    assert_ne "$stash_refs" "0" "the old clone's stash should be archived as a ref in the root"
}
run_test worktreeify_basic test_worktreeify_basic

# (11b) `link --worktreeify` relocates an initialized nested submodule's
# gitdir (under .git/modules/...) into the new worktree's administrative
# area and fixes up its gitdir-link file + core.worktree so the submodule
# keeps working. Also a regression check: --worktreeify refuses to run
# directly on a path that is itself a submodule's own checkout (git rejects
# worktree-backed submodules).
test_worktreeify_nested_submodule() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/work/wtB"
    ( cd "$T/work/wtB" && git submodule update -q --init --recursive )
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub"
    local root="$T/develop/sub"
    # Pre-create the root (and its own submodule checkout) via a plain,
    # non-worktreeify link first, keeping this test focused on gitdir
    # relocation rather than root auto-creation.
    run_borrow root "$T/work/wtB"
    assert_exit0 "$BORROW_CODE" "root creation should succeed ($BORROW_ERR)"

    local old_sub_gitdir
    old_sub_gitdir=$(git -C "$T/work/wtB/nested" rev-parse --absolute-git-dir)
    assert_contains "$old_sub_gitdir" "$T/work/wtB/.git/modules/" "setup: submodule gitdir should start out under wtB/.git/modules"

    run_borrow link "$T/work/wtB"
    assert_exit0 "$BORROW_CODE" "worktreeify should succeed ($BORROW_ERR)"
    assert_true_msg "wtB/.git should now be a worktree gitdir-link file" -- \
        grep -q '^gitdir:' "$T/work/wtB/.git"

    local admin new_sub_gitdir
    admin=$(git -C "$T/work/wtB" rev-parse --absolute-git-dir)
    assert_true_msg "admin dir should have a relocated modules/ directory" -- test -d "$admin/modules"
    assert_true_msg "old git dir's modules/ should have been moved out, not copied" -- \
        test ! -d "$T/work/wtB/.git.pre-worktreeify/modules"

    new_sub_gitdir=$(git -C "$T/work/wtB/nested" rev-parse --absolute-git-dir)
    assert_contains "$new_sub_gitdir" "$admin/modules/" "submodule gitdir should now live under the new admin modules dir"
    assert_true_msg "submodule should still be usable (rev-parse HEAD)" -- \
        bash -c 'git -C "$1" rev-parse HEAD >/dev/null' _ "$T/work/wtB/nested"
    assert_true_msg "submodule status should still work (core.worktree fixed up)" -- \
        git -C "$T/work/wtB/nested" status --porcelain

    # Linking a submodule checkout directly falls back to shared-objects
    # (alternates) form since git rejects worktree-backed submodules.
    run_borrow link "$T/work/wtB/nested"
    assert_exit0 "$BORROW_CODE" "link on a submodule checkout should succeed via alternates ($BORROW_ERR)"
    assert_file_exists "$admin/modules/nested/objects/info/alternates" "submodule should borrow via alternates"
}
run_test worktreeify_nested_submodule test_worktreeify_nested_submodule

# (11c) Regression test: `link --worktreeify` on a standalone clone whose
# root does not exist yet (so create_root must auto-create and seed it,
# including its own nested submodule checkout) used to fail outright because
# the global $worktreeify flag leaked into init_submodules' recursive
# link_borrower() calls; worktreeify is now an explicit, non-leaking
# link_borrower parameter.
test_worktreeify_creates_root_with_submodules() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/work/wtD"
    ( cd "$T/work/wtD" && git submodule update -q --init --recursive )
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub"

    # Root "develop/sub" does not exist yet -- this forces create_root (and
    # its own init_submodules seeding of the root's "nested" submodule) to
    # run inside the same process as the --worktreeify request.
    run_borrow link "$T/work/wtD"
    assert_exit0 "$BORROW_CODE" "worktreeify should succeed on first-time root auto-creation with submodules ($BORROW_ERR)"

    assert_true_msg "wtD/.git should now be a worktree gitdir-link file" -- \
        grep -q '^gitdir:' "$T/work/wtD/.git"
    local root="$T/develop/sub"
    local wt_gitdir root_admin
    wt_gitdir=$(git -C "$T/work/wtD" rev-parse --absolute-git-dir)
    root_admin=$(git -C "$root" rev-parse --absolute-git-dir)
    assert_contains "$wt_gitdir" "$root_admin/worktrees/" "wtD should have become a worktree of the auto-created root"
}
run_test worktreeify_creates_root_with_submodules test_worktreeify_creates_root_with_submodules

# (12a) An explicit `map`ping always wins over the implied default root path
# -- no prompt, even though the mapped path doesn't match the URL-derived
# candidate path at all.
test_root_mapping_wins() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt12a"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/elsewhere/custom-root"
    run_borrow link "$T/work/wt12a"
    assert_exit0 "$BORROW_CODE" "link should use the mapping without prompting ($BORROW_ERR)"
    assert_not_contains "$BORROW_ERR" "No root is mapped" "should not hit the name-mismatch prompt"
    assert_file_exists "$T/elsewhere/custom-root/.git" "root should have been created at the mapped path"
    assert_file_absent "$T/develop/subsub" "root should NOT have been created at the URL-implied default path"
}
run_test root_mapping_wins test_root_mapping_wins

# (12b) If the checkout itself is already marked borrow.isRoot=true, resolve_root
# uses it as-is without ever prompting, even though no mapping exists and the
# URL-implied default candidate path doesn't exist.
test_root_already_isroot_no_prompt() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt12b"
    git -C "$T/work/wt12b" config borrow.isRoot true
    run_borrow link "$T/work/wt12b"
    assert_exit0 "$BORROW_CODE" "link should succeed without prompting ($BORROW_ERR)"
    assert_not_contains "$BORROW_ERR" "No root is mapped" "should not hit the name-mismatch prompt"
    assert_contains "$BORROW_OUT$BORROW_ERR" "nothing to link" "an already-isRoot checkout should resolve to itself"
}
run_test root_already_isroot_no_prompt test_root_already_isroot_no_prompt

# (12c) With no mapping, if the candidate root path already exists as an
# unrelated repository, resolve_root must prompt on the terminal. In a
# harness with no controlling terminal, that prompt attempt fails closed
# (aborts, exit non-zero) without writing any mapping.
test_root_eof_no_tty_abort() {
    new_env
    git init -q "$T/develop/subsub"
    ( cd "$T/develop/subsub" && echo unrelated > u.txt && git add -A && git commit -q -m unrelated )
    git clone -q https://example.test/org/subsub.git "$T/work/wt12c"
    run_borrow link "$T/work/wt12c"
    assert_exit_nonzero "$BORROW_CODE" "link should abort when it needs to prompt on collision but has no terminal"
    assert_contains "$BORROW_ERR" "no terminal" "should explain it needs an answer but has no terminal"
    local mapped
    mapped=$(git config --global --get "borrow.https://example.test/org/subsub.git.root" 2>/dev/null) || mapped=
    assert_eq "$mapped" "" "no mapping should have been written by the aborted prompt"
}
run_test root_eof_no_tty_abort test_root_eof_no_tty_abort

# (12d) --on-collision=share / --on-collision=fail must resolve the
# candidate-exists-as-unrelated-repo collision deterministically without prompting.
test_on_collision_flags() {
    new_env
    mkdir -p "$T/develop" "$T/up/src"

    git init -q "$T/up/src/sub2"
    ( cd "$T/up/src/sub2" && echo x > x.txt && git add -A && git commit -q -m c1 )
    git clone -q --bare "$T/up/src/sub2" "$T/up/sub2.git"
    git init -q "$T/develop/sub2"
    ( cd "$T/develop/sub2" && echo y > y.txt && git add -A && git commit -q -m unrelated )
    git clone -q https://example.test/org/sub2.git "$T/work/wt12d3"
    run_borrow link --on-collision=fail "$T/work/wt12d3"
    assert_exit_nonzero "$BORROW_CODE" "--on-collision=fail should refuse the collision case"
    assert_contains "$BORROW_ERR" "shares no remote URL" "should explain the collision"

    git init -q "$T/up/src/sub3"
    ( cd "$T/up/src/sub3" && echo x > x.txt && git add -A && git commit -q -m c1 )
    git clone -q --bare "$T/up/src/sub3" "$T/up/sub3.git"
    git init -q "$T/develop/sub3"
    ( cd "$T/develop/sub3" && echo y > y.txt && git add -A && git commit -q -m unrelated )
    git clone -q https://example.test/org/sub3.git "$T/work/wt12d4"
    run_borrow link --on-collision=share "$T/work/wt12d4"
    assert_exit0 "$BORROW_CODE" "--on-collision=share should share the unrelated existing dir as root ($BORROW_ERR)"
}
run_test on_collision_flags test_on_collision_flags

run_borrow_pty() {
    local answer=$1 dir=$2 out
    shift 2
    out=$(mktemp)
    ( cd "$dir" && printf '%s\n' "$answer" | git-borrow "$@" ) >"$out" 2>&1
    BORROW_CODE=$?
    BORROW_OUT=$(cat "$out")
    BORROW_ERR=
    rm -f "$out"
}
# (12f) The collision prompt ("Press Enter to share it as the root, or type
# another root path"), for when the URL-implied candidate path already
# exists as an unrelated Git repo: drive it over a real pty with Enter
# (default: share the existing dir) and a typed alternate path.
test_root_collision_prompt_interactive() {
    new_env
    mkdir -p "$T/develop" "$T/up/src"

    git init -q "$T/up/src/subA"
    ( cd "$T/up/src/subA" && echo x > x.txt && git add -A && git commit -q -m c1 )
    git clone -q --bare "$T/up/src/subA" "$T/up/subA.git"
    git init -q "$T/develop/subA"
    ( cd "$T/develop/subA" && echo y > y.txt && git add -A && git commit -q -m unrelated )
    git clone -q https://example.test/org/subA.git "$T/work/wt12f1"
    run_borrow_pty "" "$T" link "$T/work/wt12f1"
    assert_exit0 "$BORROW_CODE" "Enter (default) should resolve the collision prompt ($BORROW_OUT)"
    assert_contains "$BORROW_OUT" "Press Enter to share it as the root" "should show the collision prompt"
    assert_eq "$(git -C "$T/develop/subA" config --bool borrow.isRoot)" "true" \
        "default should share the pre-existing dir as the root"

    git init -q "$T/up/src/subB"
    ( cd "$T/up/src/subB" && echo x > x.txt && git add -A && git commit -q -m c1 )
    git clone -q --bare "$T/up/src/subB" "$T/up/subB.git"
    git init -q "$T/develop/subB"
    ( cd "$T/develop/subB" && echo y > y.txt && git add -A && git commit -q -m unrelated )
    git clone -q https://example.test/org/subB.git "$T/work/wt12f2"
    run_borrow_pty "$T/custom/collision-path" "$T" link "$T/work/wt12f2"
    assert_exit0 "$BORROW_CODE" "a typed alternate path should resolve the collision prompt ($BORROW_OUT)"
    assert_file_exists "$T/custom/collision-path/.git" "the typed path should become the root instead of the colliding dir"
    assert_ne "$(git -C "$T/develop/subB" config --bool borrow.isRoot 2>/dev/null || echo false)" "true" \
        "the colliding pre-existing dir should be left alone when a different path is typed"
}
run_test root_collision_prompt_interactive test_root_collision_prompt_interactive

# (13) `add -r` on an existing target only recurses into submodules that are
# already initialized -- an uninitialized submodule is left alone (not
# cloned, not borrowed). Once manually initialized, `add -r` links it.
test_add_existing_target_init_opt_in() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/work/wt13a"
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub13a"
    # "nested" is declared in .gitmodules but never initialized.
    run_borrow link -r "$T/work/wt13a"
    assert_exit0 "$BORROW_CODE" "link -r should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "Skipping uninitialized submodule $T/work/wt13a/nested" \
        "should note that it skipped the uninitialized submodule"
    assert_file_absent "$T/work/wt13a/nested/.git" "the uninitialized submodule should not have been cloned"

    # -q suppresses the skip note.
    run_borrow link -r -q "$T/work/wt13a"
    assert_exit0 "$BORROW_CODE" "-q link on an existing target should succeed ($BORROW_ERR)"
    assert_not_contains "$BORROW_ERR" "Skipping" "-q should suppress the skip note"
    assert_file_absent "$T/work/wt13a/nested/.git" "nested should still be uninitialized after a quiet re-run"

    # Manually initializing the submodule and re-running link -r links it.
    ( cd "$T/work/wt13a" && git submodule update -q --init )
    run_borrow link -r "$T/work/wt13a"
    assert_exit0 "$BORROW_CODE" "link -r after manual submodule init should succeed ($BORROW_ERR)"
    assert_file_exists "$T/develop/subsub/.git" "an initialized submodule should get its own auto-created root"
    assert_eq "$(git -C "$T/work/wt13a/nested" config --bool borrow.isRoot 2>/dev/null || echo false)" "false" \
        "the submodule checkout itself should be a borrower, not a root"
}
run_test add_existing_target_init_opt_in test_add_existing_target_init_opt_in

# (13b) Partial submodule initialization invariant: when a repo has TWO
# submodules (nested and nested2) and only nested is initialized, neither
# `add -r <existing>`, auto-created root seeding, nor `add -r <new-worktree>`
# from that repo may implicitly initialize nested2.
test_add_existing_target_default_vs_init() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/scratch-sub"
    ( cd "$T/scratch-sub" && git submodule add -q https://example.test/org/subsub.git nested2 \
        && git add -A && git commit -q -m "add second submodule" && git push -q origin HEAD:main )

    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub13b"

    git clone -q https://example.test/org/sub.git "$T/work/wt13b"
    ( cd "$T/work/wt13b" && git submodule update -q --init -- nested )
    assert_file_exists "$T/work/wt13b/nested/file.txt" "setup: nested should be checked out"
    assert_file_absent "$T/work/wt13b/nested2/.git" "setup: nested2 should NOT be checked out"

    run_borrow link -r "$T/work/wt13b"
    assert_exit0 "$BORROW_CODE" "add -r should succeed on an existing target ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "Submodule already initialized: nested" "checked-out nested should be linked"
    assert_contains "$BORROW_ERR" "Skipping uninitialized submodule $T/work/wt13b/nested2" \
        "not-checked-out nested2 should be skipped, not cloned"
    assert_file_absent "$T/work/wt13b/nested2/.git" "nested2 should remain uncloned in borrower"
    assert_file_absent "$T/develop/sub13b/nested2/.git" "nested2 should also remain uncloned in the auto-created root"
    assert_true_msg "nested should be linked to a root (has a 'root' remote)" -- \
        bash -c 'git -C "$1" remote get-url root >/dev/null' _ "$T/work/wt13b/nested"

    # Creating a new worktree with -r from wt13b (which has only nested
    # initialized, not nested2) must initialize ONLY nested, leaving nested2
    # uninitialized.
    run_borrow_in "$T/work/wt13b" add -r -b nb1 "$T/wts/newwt1" main
    assert_exit0 "$BORROW_CODE" "add -r on a new worktree from partial-submodule source should succeed ($BORROW_ERR)"
    assert_file_exists "$T/wts/newwt1/nested/file.txt" "new worktree: nested (initialized in source) should be initialized"
    assert_true_msg "new worktree: nested should borrow from its root" -- \
        bash -c 'git -C "$1" remote get-url root >/dev/null' _ "$T/wts/newwt1/nested"
    assert_file_absent "$T/wts/newwt1/nested2/.git" "new worktree: nested2 (uninitialized in source) must stay uninitialized"
}
run_test add_existing_target_default_vs_init test_add_existing_target_default_vs_init

# Computes the same 12-hex borrower id git-borrow would derive for a checkout
# (sha1 of its resolved git-common-dir, first 12 chars; see borrower_id()).
borrower_id_of() {
    local common
    common=$(git -C "$1" rev-parse --path-format=absolute --git-common-dir)
    command -v cygpath >/dev/null 2>&1 && common=$(cygpath -m "$common") || common=$(cd "$common" && pwd -P)
    printf '%s' "$common" | sha1sum | cut -c1-12
}

# (14) `forget`: refuses while still borrowing without --force/--keep-refs;
# --force bypasses the safety check and drops refs too; --keep-refs bypasses
# it while preserving the archived refs; a later bare-id forget then deletes
# those archived-only refs (and --keep-refs on an archived-only id is a no-op
# error); unknown ids/paths are rejected.
test_forget_basic() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt14a"
    git -C "$T/work/wt14a" config core.worktree "$T/work/wt14a"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub14"
    local root="$T/develop/subsub14"
    run_borrow link "$T/work/wt14a"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"

    local id1
    id1=$(borrower_id_of "$T/work/wt14a")
    assert_true_msg "setup: root should have a remote.borrower/$id1 section" -- \
        bash -c 'git -C "$1" config --get "remote.borrower/$2.url" >/dev/null' -- "$root" "$id1"

    # Still borrowing (alternate present) and no --force/--keep-refs: refused.
    run_borrow forget --root "$root" "$T/work/wt14a"
    assert_exit_nonzero "$BORROW_CODE" "forget without --force/--keep-refs should refuse while still borrowing"
    assert_contains "$BORROW_ERR" "still borrows objects from" "should explain why it refused"
    assert_contains "$BORROW_ERR" "--keep-refs or --force" "should mention the escape hatches"
    assert_true_msg "remote.borrower/$id1 section should be untouched after the refusal" -- \
        bash -c 'git -C "$1" config --get "remote.borrower/$2.url" >/dev/null' -- "$root" "$id1"

    # --force bypasses the safety check; default keep_refs=0 drops the refs too.
    run_borrow forget --force --root "$root" "$T/work/wt14a"
    assert_exit0 "$BORROW_CODE" "forget --force should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "Forgot borrower $id1" "should report the forgotten id"
    assert_true_msg "remote.borrower/$id1 section should be gone" -- \
        bash -c '! git -C "$1" config --get "remote.borrower/$2.url" >/dev/null 2>&1' -- "$root" "$id1"
    local refs1
    refs1=$(git -C "$root" for-each-ref --count=1 --format=x "refs/borrowers/$id1/")
    assert_eq "$refs1" "" "forget --force (without --keep-refs) should delete the archived refs too"
    assert_true_msg "forget should not touch the borrower's own alternate file" -- \
        grep -qF "$(git -C "$root" rev-parse --path-format=absolute --git-common-dir)/objects" \
        "$T/work/wt14a/.git/objects/info/alternates"

    # --keep-refs bypasses the safety check too, but preserves the refs.
    git clone -q https://example.test/org/sub.git "$T/work/wt14b"
    git -C "$T/work/wt14b" config core.worktree "$T/work/wt14b"
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub14b"
    local root2="$T/develop/sub14b"
    run_borrow link "$T/work/wt14b"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"
    local id2
    id2=$(borrower_id_of "$T/work/wt14b")

    run_borrow forget --keep-refs --root "$root2" "$T/work/wt14b"
    assert_exit0 "$BORROW_CODE" "forget --keep-refs should succeed while still borrowing ($BORROW_ERR)"
    assert_true_msg "remote.borrower/$id2 section should be gone" -- \
        bash -c '! git -C "$1" config --get "remote.borrower/$2.url" >/dev/null 2>&1' -- "$root2" "$id2"
    local refs2
    refs2=$(git -C "$root2" for-each-ref --count=1 --format=x "refs/borrowers/$id2/")
    assert_ne "$refs2" "" "--keep-refs should preserve the archived refs"

    # The id is now "archived-only" (refs exist, no remote.borrower/$id section).
    # --keep-refs on an archived-only id is a no-op and should error.
    run_borrow forget --keep-refs --root "$root2" "$id2"
    assert_exit_nonzero "$BORROW_CODE" "forget --keep-refs on an archived-only id should refuse"
    assert_contains "$BORROW_ERR" "only archived refs" "should explain there is nothing to keep"
    assert_contains "$BORROW_ERR" "--keep-refs would leave nothing to do" "should explain why"

    # Without --keep-refs, forgetting the archived-only id deletes the refs.
    run_borrow forget --root "$root2" "$id2"
    assert_exit0 "$BORROW_CODE" "forget on an archived-only id should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "Deleted archived refs refs/borrowers/$id2/" "should report the deletion"
    local refs2b
    refs2b=$(git -C "$root2" for-each-ref --count=1 --format=x "refs/borrowers/$id2/")
    assert_eq "$refs2b" "" "archived refs should now be gone"

    # Missing borrower: neither a repo path nor a known/plausible borrower id.
    run_borrow forget --root "$root2" "not-a-real-path-or-id"
    assert_exit_nonzero "$BORROW_CODE" "forget on a bogus target should fail"
    assert_contains "$BORROW_ERR" "not a borrower id or repository: not-a-real-path-or-id" "should name the bad target"

    # Bogus id: well-formed (12 hex chars) but unknown, and not a repo path either.
    run_borrow forget --root "$root2" "deadbeefcafe"
    assert_exit_nonzero "$BORROW_CODE" "forget on a plausible-but-unknown id should fail"
    assert_contains "$BORROW_ERR" "not a borrower id or repository: deadbeefcafe" "should name the bad id"
}
run_test forget_basic test_forget_basic

# (15) `status`'s "Linked worktrees:" section across varieties: a
# git-borrow-created worktree (submodule recursively borrowed, N/N), a plain
# `git worktree add` one with a submodule initialized but NOT borrowed
# (0/N), a plain one with no initialized submodules (no counts shown) that
# is then given a fake git-rectree-origin breadcrumb, and a worktree whose
# directory was removed out from under git (MISSING).
test_status_linked_worktree_variety() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/work/wt15src"
    ( cd "$T/work/wt15src" && git submodule update -q --init )
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub15"
    run_borrow link -r "$T/work/wt15src"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"
    local root="$T/develop/sub15"
    mkdir -p "$T/wts"

    # wtA: git-borrow worktree -r -> submodule recursively borrowed (1/1).
    run_borrow_in "$root" add -r -b bA "$T/wts/wtA"
    assert_exit0 "$BORROW_CODE" "worktree -r should succeed ($BORROW_ERR)"
    assert_file_exists "$T/wts/wtA/nested/file.txt" "setup: wtA's submodule should be initialized"

    # wtB: plain `git worktree add`, submodule initialized by hand (not via
    # git-borrow), so it's never borrowed -> 0/1.
    git -C "$root" worktree add -q -b bB "$T/wts/wtB" >/dev/null
    ( cd "$T/wts/wtB" && git submodule update -q --init )
    assert_file_exists "$T/wts/wtB/nested/file.txt" "setup: wtB's submodule should be initialized (but not via git-borrow)"

    # wtC: plain `git worktree add`, submodule left uninitialized (no counts shown).
    git -C "$root" worktree add -q -b bC "$T/wts/wtC" >/dev/null

    # wtD: plain `git worktree add`, then its directory is deleted out from
    # under git (simulating external removal without `git worktree remove`).
    git -C "$root" worktree add -q -b bD "$T/wts/wtD" >/dev/null
    rm -rf "$T/wts/wtD"

    run_borrow status "$root"
    assert_exit0 "$BORROW_CODE" "status should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_OUT" "Role: root" "status on the root should say Role: root"
    assert_contains "$BORROW_OUT" "Linked worktrees:" "status should list linked worktrees"
    assert_contains "$BORROW_OUT" "$T/wts/wtA  [git borrow worktree, " "wtA should show the git-borrow-created kind"
    assert_contains "$BORROW_OUT" "; 1/1 submodules borrow]" "wtA's submodule should show as fully borrowed"
    assert_contains "$BORROW_OUT" "$T/wts/wtB  [plain; 0/1 submodules borrow]" \
        "wtB should show plain kind with an initialized-but-unborrowed submodule"
    assert_contains "$BORROW_OUT" "$T/wts/wtC  [plain]" \
        "wtC should show plain kind with no submodule counts"
    assert_contains "$BORROW_OUT" "$T/wts/wtD  [plain; MISSING (git worktree prune)]" \
        "wtD should be reported missing"
}
run_test status_linked_worktree_variety test_status_linked_worktree_variety

# (16a) A URL with no exact mapping of its own, whose normalized (host/path,
# case-folded) form matches a single existing root, automatically reuses
# that root and records a mapping for the new URL spelling.
test_canonical_suggestion_single_match_accept() {
    new_env
    git config --global --add url."file://$T/up/".insteadOf "https://EXAMPLE.test/org/"

    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub"
    git clone -q https://example.test/org/sub.git "$T/work/wt16a-orig"
    run_borrow link "$T/work/wt16a-orig"
    assert_exit0 "$BORROW_CODE" "setup: link should create the original root ($BORROW_ERR)"
    assert_file_exists "$T/develop/sub/.git" "setup: root should exist"

    git clone -q https://EXAMPLE.test/org/sub.git "$T/work/wt16a-new"
    git -C "$T/work/wt16a-new" config core.worktree "$T/work/wt16a-new"
    run_borrow link "$T/work/wt16a-new"
    assert_exit0 "$BORROW_CODE" "single canonical match should auto-reuse root ($BORROW_ERR)"
    assert_eq "$(git -C "$T/work/wt16a-new" config --get remote.root.url 2>/dev/null)" "$T/develop/sub" \
        "should reuse the existing root, not create a second one"
    local mapped
    mapped=$(git config --global --get "borrow.https://EXAMPLE.test/org/sub.git.root")
    assert_eq "$mapped" "$T/develop/sub" "a fresh exact mapping for the new URL spelling should now exist"
}
run_test canonical_suggestion_single_match_accept test_canonical_suggestion_single_match_accept
# (16c) When TWO existing (differently-URL'd) mappings both canonically
# match a third, new URL, there's no safe default: --on-collision=fail
# dies listing both candidates; --on-collision=share does NOT auto-pick
# either (falls through to normal default-path creation, i.e. a third,
# separate root); interactively, both candidates are listed and typing one
# of them uses it.
test_canonical_suggestion_multiple_roots() {
    new_env
    # Two fabricated mappings (need not exist on disk) whose canonical form
    # both equal "example.test/org/sub", under different exact URL spellings.
    git config --global "borrow.https://EXAMPLE.test/org/sub.git.root" "$T/fakeroots/a"
    git config --global "borrow.git@example.test:org/sub.git.root" "$T/fakeroots/b"

    # Each sub-case below actually resolves a root for a checkout of
    # "sub.git" and, on success, that resolution registers its own fresh
    # exact mapping (same as any `add`) -- which would poison a later
    # sub-case reusing the identical URL spelling. So each gets its own
    # distinct, but still canonically-matching, URL spelling (via its own
    # insteadOf alias) that no other sub-case touches.
    git clone -q https://example.test/org/sub.git "$T/work/wt16c-fail"
    run_borrow link --on-collision=fail "$T/work/wt16c-fail"
    assert_exit_nonzero "$BORROW_CODE" "--on-collision=fail should refuse to pick among multiple candidates"
    assert_contains "$BORROW_ERR" "$T/fakeroots/a" "should list the first candidate"
    assert_contains "$BORROW_ERR" "$T/fakeroots/b" "should list the second candidate"

    git config --global --add url."file://$T/up/".insteadOf "https://Example.Test/org/"
    git clone -q https://Example.Test/org/sub.git "$T/work/wt16c-share"
    run_borrow link --on-collision=share "$T/work/wt16c-share"
    assert_exit0 "$BORROW_CODE" "--on-collision=share should fall through instead of guessing ($BORROW_ERR)"
    assert_file_exists "$T/develop/sub/.git" "share should fall through to the normal default-path root"
    assert_file_absent "$T/fakeroots/a" "share should not silently pick the first candidate"
    assert_file_absent "$T/fakeroots/b" "share should not silently pick the second candidate"

    git config --global --add url."file://$T/up/".insteadOf "https://EXAMPLE.TEST/org/"
    git clone -q https://EXAMPLE.TEST/org/sub.git "$T/work/wt16c-interactive"
    run_borrow_pty "$T/fakeroots/b" "$T" link "$T/work/wt16c-interactive"
    assert_exit0 "$BORROW_CODE" "typing one of the listed candidates should succeed ($BORROW_OUT)"
    assert_contains "$BORROW_OUT" "$T/fakeroots/a" "should list the first candidate"
    assert_contains "$BORROW_OUT" "$T/fakeroots/b" "should list the second candidate"
    assert_file_exists "$T/fakeroots/b/.git" "typing the second candidate should create/use the root there"
}
run_test canonical_suggestion_multiple_roots test_canonical_suggestion_multiple_roots

# (16e) `status -g`/`--global`: lists every known URL->root mapping, then
# each distinct root's own borrowers (a root with no borrowers shows
# "(none)"), including a root that has two different mapped URLs.
test_status_global() {
    new_env

    git config --global "borrow.https://example.test/org/parent.git.root" "$T/develop/parent"
    git clone -q https://example.test/org/parent.git "$T/work/wt16e-g1"
    git -C "$T/work/wt16e-g1" config core.worktree "$T/work/wt16e-g1"
    run_borrow link "$T/work/wt16e-g1"
    assert_exit0 "$BORROW_CODE" "setup: link should create root1 ($BORROW_ERR)"
    assert_file_exists "$T/develop/parent/.git" "setup: root1 should exist"
    # A second, differently-cased exact URL mapped to the very same root.
    git config --global "borrow.https://EXAMPLE.test/org/parent.git.root" "$T/develop/parent"

    # Root2: a submodule-free one-off repo, cloned directly at its own
    # default path and added in place, so it becomes its own root with
    # genuinely zero borrowers (root1 above has submodules of its own,
    # whose auto-bootstrapped roots pick up a borrower from root1's own
    # checkout, so they wouldn't demonstrate a truly empty root).
    git init -q "$T/up/src/leaf"
    ( cd "$T/up/src/leaf" && echo x > x.txt && git add -A && git commit -q -m c1 )
    git clone -q --bare "$T/up/src/leaf" "$T/up/leaf.git"
    git clone -q https://example.test/org/leaf.git "$T/develop/leaf"
    run_borrow link "$T/develop/leaf"
    assert_exit0 "$BORROW_CODE" "setup: add should mark root2 in place ($BORROW_ERR)"
    assert_eq "$(git -C "$T/develop/leaf" config --bool borrow.isRoot)" "true" "setup: root2 should be isRoot"

    run_borrow status -g
    assert_exit0 "$BORROW_CODE" "status -g should succeed ($BORROW_ERR)"
    assert_contains "$BORROW_OUT" "Global root mappings:" "should show the mappings header"
    assert_contains "$BORROW_OUT" "https://example.test/org/parent.git -> $T/develop/parent" \
        "should list root1's original mapping"
    assert_contains "$BORROW_OUT" "https://EXAMPLE.test/org/parent.git -> $T/develop/parent" \
        "should list root1's second mapping"
    assert_contains "$BORROW_OUT" "https://example.test/org/leaf.git -> $T/develop/leaf" \
        "should list root2's mapping"
    assert_contains "$BORROW_OUT" "$T/develop/parent:" "should show root1's own header"
    assert_contains "$BORROW_OUT" "$T/develop/leaf:" "should show root2's own header"
    assert_contains "$BORROW_OUT" "$T/work/wt16e-g1" "root1's section should list its real borrower"
    assert_contains "$(printf '%s\n' "$BORROW_OUT")" "$T/develop/leaf:
  Borrowers:
    (none)" "root2's section should show zero borrowers"

    run_borrow status --global
    assert_exit0 "$BORROW_CODE" "status --global should also succeed ($BORROW_ERR)"
    assert_contains "$BORROW_OUT" "Global root mappings:" "--global should behave the same as -g"
}
run_test status_global test_status_global

# (16f) The `map` subcommand is gone; it's now just an unrecognized command.
test_map_command_removed() {
    new_env
    run_borrow map
    assert_exit_nonzero "$BORROW_CODE" "map should no longer be a recognized command"
    assert_contains "$BORROW_ERR" "unknown command" "should report it as an unknown command"

    run_borrow map https://example.test/org/sub.git "$T/develop/sub"
    assert_exit_nonzero "$BORROW_CODE" "map with args should also no longer be recognized"
    assert_contains "$BORROW_ERR" "unknown command" "should report it as an unknown command"
}
run_test map_command_removed test_map_command_removed

lfs_media_dir_of() {
    git -C "$1" lfs env 2>/dev/null | sed -n 's/^LocalMediaDir=//p' | tail -n 1
}

# (18) LFS hardlink pooling (the default --lfs=hardlink mode): linking a
# borrower hardlinks (`cp -al`) its unique LFS media objects into the root's
# media pool, and hardlinks the root's full pool back into the borrower;
# a second borrower of the same root picks up what's already pooled there;
# and a borrower that already had a local duplicate of a pooled object gets
# that duplicate relinked (reclaiming the space) rather than left as a
# separate copy. No real LFS remote/smudge is needed for this -- `git lfs
# env` (and thus the media directory) works in any repo with git-lfs
# installed, tracked content or not.
test_lfs_hardlink_pooling() {
    command -v git-lfs >/dev/null 2>&1 || { printf 'SKIP: git-lfs not installed\n' >&2; return 0; }
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt18a"
    git -C "$T/work/wt18a" config core.worktree "$T/work/wt18a"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub18"
    local root="$T/develop/subsub18"

    # Seed a fake pre-existing LFS media object in wt18a before its first link.
    local media_a
    media_a=$(lfs_media_dir_of "$T/work/wt18a")
    mkdir -p "$media_a/ab"
    printf 'unique-to-wt18a' > "$media_a/ab/wt18a-unique.bin"

    run_borrow link "$T/work/wt18a"
    assert_exit0 "$BORROW_CODE" "link should succeed ($BORROW_ERR)"
    local media_root
    media_root=$(lfs_media_dir_of "$root")
    assert_file_exists "$media_root/ab/wt18a-unique.bin" "wt18a's unique LFS object should be pooled into the root"
    assert_true_msg "root's and wt18a's copies should be the same hardlinked inode" -- \
        test "$media_root/ab/wt18a-unique.bin" -ef "$media_a/ab/wt18a-unique.bin"

    # Plant an object directly in the root's pool that neither borrower has
    # yet, so "wt18b picks it up from the pool" is distinct from the
    # wt18a-vs-wt18b duplicate-reclaim case checked below.
    mkdir -p "$media_root/ef"
    printf 'unique-to-root-only' > "$media_root/ef/root-only.bin"

    # A second borrower of the same root, with its own distinct pre-existing
    # object, plus a duplicate of wt18a's object (same relative path/content)
    # that predates linking -- that duplicate should get reclaimed (relinked)
    # rather than kept as a separate copy.
    git clone -q https://example.test/org/subsub.git "$T/work/wt18b"
    git -C "$T/work/wt18b" config core.worktree "$T/work/wt18b"
    local media_b
    media_b=$(lfs_media_dir_of "$T/work/wt18b")
    mkdir -p "$media_b/cd" "$media_b/ab"
    printf 'unique-to-wt18b' > "$media_b/cd/wt18b-unique.bin"
    printf 'unique-to-wt18a' > "$media_b/ab/wt18a-unique.bin"

    run_borrow link "$T/work/wt18b"
    assert_exit0 "$BORROW_CODE" "second link should succeed ($BORROW_ERR)"
    assert_file_exists "$media_root/cd/wt18b-unique.bin" "wt18b's unique LFS object should also be pooled into the root"
    assert_true_msg "wt18b should pick up the root-only pooled object (hardlinked, not copied)" -- \
        test "$media_root/ef/root-only.bin" -ef "$media_b/ef/root-only.bin"

    assert_true_msg "wt18b's pre-existing duplicate of a pooled object should be reclaimed (relinked to the root's inode)" -- \
        test "$media_root/ab/wt18a-unique.bin" -ef "$media_b/ab/wt18a-unique.bin"
    assert_eq "$(cat "$media_b/ab/wt18a-unique.bin")" "unique-to-wt18a" \
        "wt18b's reclaimed duplicate should keep its original content"

    # wt18a was linked before wt18b existed, so it does not retroactively
    # gain wt18b's object until it is linked (refreshed) again.
    assert_file_absent "$media_a/cd/wt18b-unique.bin" "wt18a should not yet have wt18b's object (not relinked since)"
    run_borrow link "$T/work/wt18a"
    assert_exit0 "$BORROW_CODE" "re-link should succeed ($BORROW_ERR)"
    assert_true_msg "re-linking wt18a should now pull in wt18b's pooled object too" -- \
        test "$media_root/cd/wt18b-unique.bin" -ef "$media_a/cd/wt18b-unique.bin"
}
run_test lfs_hardlink_pooling test_lfs_hardlink_pooling

# (19) `add` in a repo creates a worktree, and falls back to --detach with a
# warning if the requested branch is already checked out.
test_add_from_local_repo_absolute_target() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/other-checkout"
    mkdir -p "$T/elsewhere"

    run_borrow_in "$T/other-checkout" add -b fromb1 "$T/elsewhere/wt-rel" main
    assert_exit0 "$BORROW_CODE" "add in repo should succeed ($BORROW_ERR)"
    assert_file_exists "$T/elsewhere/wt-rel/file.txt" "target should be created"

    # Branch collision fallback to --detach when requesting already checked-out 'main':
    run_borrow_in "$T/other-checkout" add "$T/elsewhere/wt-coll" main
    assert_exit0 "$BORROW_CODE" "add with colliding branch should fall back to --detach ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "falling back to --detach" "should warn about --detach fallback"
    assert_file_exists "$T/elsewhere/wt-coll/file.txt" "colliding target should still be checked out detached"
}
run_test add_from_local_repo_absolute_target test_add_from_local_repo_absolute_target

# (20) `clone`: resolves/creates the root for <url>, creates <dir> as a linked
# worktree of the root, supports -b, --detach, and recursive submodules (-r),
# and falls back to --detach on branch collision.
test_add_from_url_plain_clone() {
    new_env
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub"

    run_borrow clone -r -b topic1 https://example.test/org/sub.git "$T/work/wt-clone1"
    assert_exit0 "$BORROW_CODE" "clone -r -b should succeed ($BORROW_ERR)"
    assert_file_exists "$T/develop/sub/.git" "clone should auto-create the root"
    assert_true_msg "wt-clone1/.git should be a worktree gitdir-link file" -- \
        grep -q '^gitdir:' "$T/work/wt-clone1/.git"
    assert_eq "$(git -C "$T/work/wt-clone1" symbolic-ref --short HEAD)" "topic1" "wt-clone1 should be on topic1"
    assert_file_exists "$T/work/wt-clone1/nested/file.txt" "clone -r should initialize and borrow submodule"

    # Second clone when root already exists, requesting already-checked-out branch 'main':
    run_borrow clone -b main https://example.test/org/sub.git "$T/work/wt-clone2"
    assert_exit0 "$BORROW_CODE" "second clone on colliding branch 'main' should fall back to --detach ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "checked out elsewhere; leaving" "should warn about branch collision"
    assert_file_exists "$T/work/wt-clone2/file.txt" "wt-clone2 should be checked out"
}
run_test add_from_url_plain_clone test_add_from_url_plain_clone

# (21) Bad combinations: `add` on an existing target; `add` outside a repo;
# `link` on a non-repo or nonexistent path.
test_add_rejects_bad_combinations() {
    new_env
    git clone -q https://example.test/org/subsub.git "$T/work/wt-exists"

    run_borrow_in "$T/work/wt-exists" add "$T/work/wt-exists"
    assert_exit_nonzero "$BORROW_CODE" "add on an existing repo should fail and suggest link"
    assert_contains "$BORROW_ERR" "target already exists" "error should explain target already exists"

    mkdir -p "$T/no-repo-here" "$T/existing-non-repo"
    run_borrow_in "$T/no-repo-here" add "$T/no-repo-here/brand-new"
    assert_exit_nonzero "$BORROW_CODE" "add outside a repo should fail"
    assert_contains "$BORROW_ERR" "not in a Git repository" "error should explain not in a Git repository"

    run_borrow link "$T/existing-non-repo"
    assert_exit_nonzero "$BORROW_CODE" "link on a non-repo directory should fail"
    assert_contains "$BORROW_ERR" "not a Git repository" "error should explain target is not a Git repository"

    run_borrow link "$T/work/does-not-exist"
    assert_exit_nonzero "$BORROW_CODE" "link on a nonexistent path should fail"
    assert_contains "$BORROW_ERR" "not a Git repository" "error should explain target is not a Git repository"
}
run_test add_rejects_bad_combinations test_add_rejects_bad_combinations

# (22) Submodule init avoids the network whenever a local source (the root)
# already has the exact pinned commit: with the submodule's real upstream
# entirely removed (so any real network clone attempt would fail), `add -r
# --init` still succeeds by cloning from the local root, and afterward the
# submodule's own "origin" remote is fixed back to the real (now-broken) URL
# rather than left pointing at the local root. When the root does NOT have
# the needed commit, it falls back to a real (working) network clone
# instead, with a warning.
test_add_submodule_clone_avoids_network() {
    new_env
    # Seed the root for "subsub" with the exact commit "sub" pins, via an
    # already-initialized checkout linked first.
    git clone -q https://example.test/org/sub.git "$T/seed-sub"
    ( cd "$T/seed-sub" && git submodule update -q --init )
    git config --global "borrow.https://example.test/org/sub.git.root" "$T/develop/sub19"
    run_borrow link -r "$T/seed-sub"
    assert_exit0 "$BORROW_CODE" "setup: seeding the subsub root should succeed ($BORROW_ERR)"
    assert_file_exists "$T/develop/subsub/.git" "setup: subsub root should now exist with the pinned commit"

    # Add a new worktree from seed-sub (which has "nested" initialized) with
    # its real upstream (subsub.git) removed entirely -- any network clone
    # attempt against it would fail.
    mv "$T/up/subsub.git" "$T/up/subsub.git.disabled"

    run_borrow_in "$T/seed-sub" add -r -b b19a "$T/work/wt19a"
    assert_exit0 "$BORROW_CODE" "add -r should succeed using the local root, never touching the (now-removed) real upstream ($BORROW_ERR)"
    assert_file_exists "$T/work/wt19a/nested/file.txt" "nested should be populated from the local root"
    local origin_url
    origin_url=$(git -C "$T/work/wt19a/nested" config --get remote.origin.url)
    assert_eq "$origin_url" "https://example.test/org/subsub.git" \
        "nested's origin remote should end up pointing at the real URL"
    assert_ne "$origin_url" "$T/develop/subsub" "nested's origin should not be left pointing at the local root"

    mv "$T/up/subsub.git.disabled" "$T/up/subsub.git"

    # Now make the root NOT have the needed commit: advance "nested"'s pinned
    # commit in a fresh clone of "sub" and push it, so the (unmodified) root
    # created above is one commit behind what's now pinned.
    git clone -q https://example.test/org/sub.git "$T/advance-src"
    ( cd "$T/advance-src" && git submodule update -q --init \
        && ( cd nested && echo more >> file.txt && git add -A && git commit -q -m "subsub c2" && git push -q origin HEAD:main ) \
        && git add nested && git commit -q -m "advance nested" && git push -q origin HEAD:main )

    run_borrow_in "$T/advance-src" add -r -b b19b "$T/work/wt19b"
    assert_exit0 "$BORROW_CODE" "add -r should still succeed via network fallback when the root lacks the commit ($BORROW_ERR)"
    assert_contains "$BORROW_ERR" "doesn't have the commit nested needs; cloning from the network instead" \
        "should warn it fell back to a real network clone"
    assert_file_exists "$T/work/wt19b/nested/file.txt" "nested should still be populated via network fallback"
}
run_test add_submodule_clone_avoids_network test_add_submodule_clone_avoids_network

# (23) classify_source precedence, observed through `add`/`root`: a string
# that looks like a local path (./ or /) but doesn't exist as a directory
# produces a clear "directory does not exist" error, not a confusing
# "not a recognized URL" message. The other two precedence cases -- an
# existing local repo wins over any URL-like reading; a nonexistent string in
# recognized URL form is treated as a URL -- are already exercised throughout
# the suite via --from's "existing local repo" tests and the fixture's
# https://example.test/... URL mapping, so are not duplicated here.
test_classify_source_precedence() {
    new_env
    run_borrow_in "$T" clone ./does-not-exist-anywhere "$T/work/wt-classify"
    assert_exit_nonzero "$BORROW_CODE" "--from with a nonexistent local-looking path should fail"
    assert_contains "$BORROW_ERR" "directory does not exist: ./does-not-exist-anywhere" \
        "error should clearly say the directory does not exist, not a URL parsing complaint"
}
run_test classify_source_precedence test_classify_source_precedence

# (24) `submodule [add]` via URL: auto-creates root, adds entry to .gitmodules,
# submodule borrows objects from its root via alternates, borrower registered in
# root, supports -b and --name, and works without explicit 'add' subcommand.
test_submodule_from_url() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/super1"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub"

    # Create submodule via URL with custom name and branch
    run_borrow_in "$T/super1" submodule add -b main --name custom-subsub https://example.test/org/subsub.git "lib/my-subsub"
    assert_exit0 "$BORROW_CODE" "submodule add should succeed ($BORROW_ERR)"

    assert_file_exists "$T/develop/subsub/.git" "root for subsub should be auto-created"
    assert_file_exists "$T/super1/lib/my-subsub/file.txt" "submodule files should be checked out"
    assert_true_msg "submodule .git should be a gitdir-link file" -- \
        grep -q '^gitdir:' "$T/super1/lib/my-subsub/.git"

    local sub_gitdir
    sub_gitdir=$(git -C "$T/super1/lib/my-subsub" rev-parse --absolute-git-dir)
    assert_contains "$sub_gitdir" "$T/super1/.git/modules/custom-subsub" \
        "submodule gitdir should live under .git/modules/custom-subsub"

    # Alternates point to root
    local alt_content
    alt_content=$(cat "$sub_gitdir/objects/info/alternates")
    assert_contains "$alt_content" "$T/develop/subsub/.git/objects" \
        "submodule alternates should point to root objects"

    # Staged in index
    local staged
    staged=$(git -C "$T/super1" diff --cached --name-only)
    assert_contains "$staged" ".gitmodules" ".gitmodules should be staged"
    assert_contains "$staged" "lib/my-subsub" "submodule gitlink should be staged"

    # Status shows borrower
    run_borrow status "$T/super1"
    assert_contains "$BORROW_OUT" "lib/my-subsub: borrower of $T/develop/subsub" \
        "superproject status should report submodule as borrower"

    # Test syntax without 'add' keyword
    run_borrow_in "$T/super1" submodule https://example.test/org/subsub.git "lib/second-subsub"
    assert_exit0 "$BORROW_CODE" "submodule without 'add' keyword should also succeed ($BORROW_ERR)"
    assert_file_exists "$T/super1/lib/second-subsub/file.txt" "second submodule should be checked out"
}
run_test submodule_from_url test_submodule_from_url

# (25) `submodule [add]` from existing PRIMARY worktree:
# Standalone clone inside superproject is converted into an object-borrower submodule;
# .git directory is absorbed into superproject's .git/modules/<name>;
# uncommitted changes and untracked files are preserved; objects are deduped.
test_submodule_from_existing_primary_worktree() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/super2"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub25"

    # Clone standalone repo inside super2 at "vendor/subsub"
    git clone -q https://example.test/org/subsub.git "$T/super2/vendor/subsub"
    echo "uncommitted edit" >> "$T/super2/vendor/subsub/file.txt"
    echo "untracked" > "$T/super2/vendor/subsub/newfile.txt"

    assert_true_msg "vendor/subsub/.git should initially be a directory" -- \
        [ -d "$T/super2/vendor/subsub/.git" ]

    # Convert it to a submodule
    run_borrow_in "$T/super2" submodule add vendor/subsub
    assert_exit0 "$BORROW_CODE" "submodule add on existing primary worktree should succeed ($BORROW_ERR)"

    assert_true_msg "vendor/subsub/.git should now be a gitdir-link file" -- \
        [ -f "$T/super2/vendor/subsub/.git" ]
    assert_true_msg "vendor/subsub/.git should contain gitdir:" -- \
        grep -q '^gitdir:' "$T/super2/vendor/subsub/.git"

    local sub_gitdir
    sub_gitdir=$(git -C "$T/super2/vendor/subsub" rev-parse --absolute-git-dir)
    assert_contains "$sub_gitdir" "$T/super2/.git/modules/vendor/subsub" \
        "absorbed gitdir should live under .git/modules/vendor/subsub"

    # Check alternates point to root
    local alt_content
    alt_content=$(cat "$sub_gitdir/objects/info/alternates")
    assert_contains "$alt_content" "$T/develop/subsub25/.git/objects" \
        "alternates should point to auto-created root objects"

    # Verify working tree modifications and untracked files survived
    local sub_status
    sub_status=$(git -C "$T/super2/vendor/subsub" status --porcelain)
    assert_contains "$sub_status" "M file.txt" "uncommitted modification should be preserved"
    assert_contains "$sub_status" "?? newfile.txt" "untracked file should be preserved"

    # Verify superproject has it staged
    local staged
    staged=$(git -C "$T/super2" diff --cached --name-only)
    assert_contains "$staged" ".gitmodules" ".gitmodules should be staged"
    assert_contains "$staged" "vendor/subsub" "submodule should be staged"
}
run_test submodule_from_existing_primary_worktree test_submodule_from_existing_primary_worktree

# (26) `submodule [add]` from existing SECONDARY worktree:
# A linked worktree (created via worktree add) is converted into an object-borrower submodule;
# worktree is pruned from the parent repo; submodule gitdir is created under .git/modules;
# working tree files and HEAD are preserved.
test_submodule_from_existing_secondary_worktree() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/super3"
    git clone -q https://example.test/org/subsub.git "$T/seed-subsub3"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub26"

    # Create secondary worktree inside super3
    git -C "$T/seed-subsub3" worktree add -q -b sec-branch "$T/super3/secondary-sub"
    assert_true_msg "secondary-sub/.git should be a gitdir-link file pointing to seed-subsub3" -- \
        grep -q 'seed-subsub3' "$T/super3/secondary-sub/.git"

    local sec_head
    sec_head=$(git -C "$T/super3/secondary-sub" rev-parse HEAD)

    # Convert secondary worktree to submodule
    run_borrow_in "$T/super3" submodule add secondary-sub
    assert_exit0 "$BORROW_CODE" "submodule add on secondary worktree should succeed ($BORROW_ERR)"

    # It should no longer be listed as a worktree of seed-subsub3
    local wt_list
    wt_list=$(git -C "$T/seed-subsub3" worktree list)
    assert_not_contains "$wt_list" "secondary-sub" "secondary-sub should be pruned from seed-subsub3 worktrees"

    # Gitdir link now points to super3's modules dir
    assert_not_contains "$(cat "$T/super3/secondary-sub/.git")" "seed-subsub3" \
        "secondary-sub/.git should no longer point to seed-subsub3"
    local sub_gitdir
    sub_gitdir=$(git -C "$T/super3/secondary-sub" rev-parse --absolute-git-dir)
    assert_contains "$sub_gitdir" "$T/super3/.git/modules/secondary-sub" \
        "submodule gitdir should now live under super3/.git/modules/secondary-sub"

    # HEAD and files preserved
    assert_eq "$(git -C "$T/super3/secondary-sub" rev-parse HEAD)" "$sec_head" \
        "HEAD commit should match original secondary worktree HEAD"
    assert_file_exists "$T/super3/secondary-sub/file.txt" "files should be present"

    # Registered as borrower of root
    local alt_content
    alt_content=$(cat "$sub_gitdir/objects/info/alternates")
    assert_contains "$alt_content" "$T/develop/subsub26/.git/objects" \
        "alternates should point to root objects"
}
run_test submodule_from_existing_secondary_worktree test_submodule_from_existing_secondary_worktree

# (27) `submodule [add]` error cases:
# outside git repo, destination outside superproject, invalid/missing arguments.
test_submodule_error_cases() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/super4"

    # Outside a repo
    mkdir -p "$T/outside"
    run_borrow_in "$T/outside" submodule add https://example.test/org/subsub.git
    assert_exit_nonzero "$BORROW_CODE" "submodule outside a repo should fail"
    assert_contains "$BORROW_ERR" "not in a Git repository" "error should note not in a Git repository"

    # No arguments
    run_borrow_in "$T/super4" submodule add
    assert_exit_nonzero "$BORROW_CODE" "submodule add with no args should fail"
    assert_contains "$BORROW_ERR" "usage:" "error should print usage"

    # Destination outside superproject
    run_borrow_in "$T/super4" submodule add https://example.test/org/subsub.git "$T/outside/subsub"
    assert_exit_nonzero "$BORROW_CODE" "submodule destination outside repo should fail"
    assert_contains "$BORROW_ERR" "submodule path must be inside repository" "error should note path must be inside repo"

    # Nonexistent local path
    run_borrow_in "$T/super4" submodule add ./does-not-exist
    assert_exit_nonzero "$BORROW_CODE" "submodule with nonexistent local path should fail"
    assert_contains "$BORROW_ERR" "directory does not exist: ./does-not-exist" "error should report directory does not exist"

    # Unsupported subcommand
    run_borrow_in "$T/super4" submodule update
    assert_exit_nonzero "$BORROW_CODE" "unsupported submodule subcommand should fail"
    assert_contains "$BORROW_ERR" "unsupported submodule command: update" "error should report unsupported subcommand"
}
run_test submodule_error_cases test_submodule_error_cases

# (28) `submodule [add]` from external tree:
# Moving an external primary repo into the superproject and converting it to a submodule.
test_submodule_from_external_tree() {
    new_env
    git clone -q https://example.test/org/sub.git "$T/super5"
    git clone -q https://example.test/org/subsub.git "$T/external-prim"
    git config --global "borrow.https://example.test/org/subsub.git.root" "$T/develop/subsub28"

    run_borrow_in "$T/super5" submodule add "$T/external-prim" "vendor/external-sub"
    assert_exit0 "$BORROW_CODE" "submodule add from external tree should succeed ($BORROW_ERR)"

    assert_file_absent "$T/external-prim" "external tree directory should be moved into superproject"
    assert_file_exists "$T/super5/vendor/external-sub/file.txt" "submodule should be at target path"
    assert_true_msg "submodule .git should be a gitdir-link file" -- \
        [ -f "$T/super5/vendor/external-sub/.git" ]

    local sub_gitdir
    sub_gitdir=$(git -C "$T/super5/vendor/external-sub" rev-parse --absolute-git-dir)
    assert_contains "$sub_gitdir" "$T/super5/.git/modules/vendor/external-sub" \
        "gitdir should be inside superproject modules"

    local alt_content
    alt_content=$(cat "$sub_gitdir/objects/info/alternates")
    assert_contains "$alt_content" "$T/develop/subsub28/.git/objects" \
        "alternates should point to root"
}
run_test submodule_from_external_tree test_submodule_from_external_tree

# (29) `submodule [add]` with --root and -r (recursive submodule init):
test_submodule_recursive_and_explicit_root() {
    new_env
    git clone -q https://example.test/org/parent.git "$T/super6"
    # Create an explicit root for sub (which has nested subsub)
    run_borrow root https://example.test/org/sub.git
    local explicit_sub_root="$BORROW_OUT"

    # Add submodule pointing to explicit root with -r
    run_borrow_in "$T/super6" submodule add -r --root "$explicit_sub_root" https://example.test/org/sub.git "lib/explicit-sub"
    assert_exit0 "$BORROW_CODE" "submodule add with --root and -r should succeed ($BORROW_ERR)"

    assert_file_exists "$T/super6/lib/explicit-sub/file.txt" "submodule files should exist"
    assert_file_exists "$T/super6/lib/explicit-sub/nested/file.txt" "recursive submodule should be initialized"

    run_borrow status "$T/super6/lib/explicit-sub"
    assert_contains "$BORROW_OUT" "Role: borrower" "submodule should be a borrower"
    assert_contains "$BORROW_OUT" "$explicit_sub_root" "submodule should borrow from explicit root"
}
run_test submodule_recursive_and_explicit_root test_submodule_recursive_and_explicit_root

# ------------------------------------------------------------- summary ----

printf '\n==== %d/%d tests passed ====\n' "$PASS_COUNT" "$TEST_COUNT"
if [ "${#FAILURES[@]}" -gt 0 ]; then
    printf 'Failures:\n'
    printf '  %s\n' "${FAILURES[@]}"
    exit 1
fi
exit 0
