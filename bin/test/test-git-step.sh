#!/usr/bin/env bash
# Test suite for git-step. Run: bash bin/test/test-git-step.sh
set -u

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
BIN_DIR=$(cd "$SCRIPT_DIR/.." && pwd)
STEP="$BIN_DIR/git-step"

FAILURES=()
PASS_COUNT=0
TEST_COUNT=0
CURRENT_TEST=""
TEMP_DIRS=()

cleanup() {
    [ "${#TEMP_DIRS[@]}" -eq 0 ] || rm -rf "${TEMP_DIRS[@]}"
}
trap cleanup EXIT

fail() {
    FAILURES+=("$CURRENT_TEST: $*")
    printf 'FAIL [%s]: %s\n' "$CURRENT_TEST" "$*" >&2
}

assert_eq() {
    local actual="$1" expected="$2" msg="${3:-}"
    if [ "$actual" != "$expected" ]; then
        fail "${msg:-assert_eq} (expected [$expected], got [$actual])"
    fi
}

assert_contains() {
    local haystack="$1" needle="$2" msg="${3:-}"
    if [[ "$haystack" != *"$needle"* ]]; then
        fail "${msg:-assert_contains} (needle [$needle] not found in [$haystack])"
    fi
}

assert_exit_code() {
    local actual="$1" expected="$2" msg="${3:-}"
    if [ "$actual" -ne "$expected" ]; then
        fail "${msg:-assert_exit_code} (expected exit code $expected, got $actual)"
    fi
}

run_test() {
    local fn="$1"
    CURRENT_TEST="$fn"
    TEST_COUNT=$((TEST_COUNT + 1))
    local prev_fail_count="${#FAILURES[@]}"
    "$fn"
    if [ "${#FAILURES[@]}" -eq "$prev_fail_count" ]; then
        PASS_COUNT=$((PASS_COUNT + 1))
        printf 'ok - %s\n' "$fn"
    fi
}

make_temp_repo() {
    local td
    td=$(mktemp -d 2>/dev/null || mktemp -d -t 'steptest')
    TEMP_DIRS+=("$td")
    (
        cd "$td"
        git init -q
        git config user.name "Test User"
        git config user.email "test@example.com"
        echo "base" > file.txt
        git add file.txt
        git commit -q -m "initial commit"
    )
    echo "$td"
}

# --- Tests ---

test_help_and_usage() {
    local out
    out=$(bash "$STEP" --help)
    assert_contains "$out" "usage: git-step" "help contains usage"
    assert_contains "$out" "continue" "help mentions continue"
    assert_contains "$out" "abort" "help mentions abort"
    assert_contains "$out" "skip" "help mentions skip"
    assert_contains "$out" "quit" "help mentions quit"
}

test_clean_state() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "No operation in progress" "clean status"

        set +e
        local cont_out cont_code
        cont_out=$(bash "$STEP" continue 2>&1)
        cont_code=$?
        assert_exit_code "$cont_code" 1 "continue exits 1 on clean state"
        assert_contains "$cont_out" "no operation in progress" "continue error message"

        local abort_out abort_code
        abort_out=$(bash "$STEP" abort 2>&1)
        abort_code=$?
        assert_exit_code "$abort_code" 1 "abort exits 1 on clean state"
    )
}

test_unmerged_hint() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        echo "change 1" > file.txt
        git commit -q -am "c1"
        git branch other
        echo "change 2" > file.txt
        git commit -q -am "c2"
        git checkout -q other
        echo "change 3" > file.txt
        git commit -q -am "c3"
        # Cause conflict
        git merge -q --no-commit main 2>/dev/null || true
        # Remove MERGE_HEAD to simulate an unmerged state with no active operation (like stash pop)
        local git_dir
        git_dir=$(git rev-parse --git-dir)
        rm -f "$git_dir/MERGE_HEAD"

        set +e
        local out code
        out=$(bash "$STEP" continue 2>&1)
        code=$?
        assert_exit_code "$code" 1 "continue exits 1 on unmerged"
        assert_contains "$out" "unmerged conflicts exist" "unmerged hint present"
    )
}

test_rebase_state() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        echo "main change" > file.txt
        git commit -q -am "main commit"
        git checkout -q -b feature HEAD~1
        echo "feature change 1" > file.txt
        git commit -q -am "feature commit 1"

        # Start rebase which conflicts
        git rebase main 2>/dev/null || true

        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "rebase" "detects rebase"

        local dry_cont
        dry_cont=$(bash "$STEP" continue -n)
        assert_eq "$dry_cont" "Would run: git rebase --continue" "rebase continue dry-run"

        local dry_args
        dry_args=$(bash "$STEP" continue -n --no-edit)
        assert_eq "$dry_args" "Would run: git rebase --continue --no-edit" "rebase extra args forwarding"

        local dry_skip
        dry_skip=$(bash "$STEP" skip -n)
        assert_eq "$dry_skip" "Would run: git rebase --skip" "rebase skip dry-run"

        local dry_quit
        dry_quit=$(bash "$STEP" quit -n)
        assert_eq "$dry_quit" "Would run: git rebase --quit" "rebase quit dry-run"

        # Now actually abort
        bash "$STEP" abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "rebase abort succeeds"
    )
}

test_merge_state() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        echo "main change" > file.txt
        git commit -q -am "main commit"
        git checkout -q -b branch HEAD~1
        echo "branch change" > file.txt
        git commit -q -am "branch commit"

        # Start merge which conflicts
        git merge main 2>/dev/null || true

        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "merge" "detects merge"

        local dry_cont
        dry_cont=$(bash "$STEP" continue -n)
        assert_eq "$dry_cont" "Would run: git merge --continue" "merge continue dry-run"

        local dry_quit
        dry_quit=$(bash "$STEP" quit -n)
        assert_eq "$dry_quit" "Would run: git merge --quit" "merge quit dry-run"

        set +e
        local skip_out skip_code
        skip_out=$(bash "$STEP" skip 2>&1)
        skip_code=$?
        assert_exit_code "$skip_code" 1 "merge skip exits 1"
        assert_contains "$skip_out" "does not support skip" "merge skip rejected"

        # Now actually abort
        bash "$STEP" abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "merge abort succeeds"
    )
}

test_cherry_pick_state() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        echo "main change" > file.txt
        git commit -q -am "main commit"
        git checkout -q -b branch HEAD~1
        echo "branch change" > file.txt
        git commit -q -am "branch commit"

        # Cherry-pick main which conflicts
        git cherry-pick main 2>/dev/null || true

        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "cherry-pick" "detects cherry-pick"

        local dry_cont
        dry_cont=$(bash "$STEP" continue -n)
        assert_eq "$dry_cont" "Would run: git cherry-pick --continue" "cherry-pick continue dry-run"

        local dry_skip
        dry_skip=$(bash "$STEP" skip -n)
        assert_eq "$dry_skip" "Would run: git cherry-pick --skip" "cherry-pick skip dry-run"

        local dry_quit
        dry_quit=$(bash "$STEP" quit -n)
        assert_eq "$dry_quit" "Would run: git cherry-pick --quit" "cherry-pick quit dry-run"

        # Actually abort
        bash "$STEP" abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "cherry-pick abort succeeds"
    )
}

test_revert_state() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        echo "step 1" > file.txt
        git commit -q -am "step 1"
        echo "step 2" > file.txt
        git commit -q -am "step 2"

        # Revert initial commit to cause conflict
        git revert HEAD~1 2>/dev/null || true

        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "revert" "detects revert"

        local dry_cont
        dry_cont=$(bash "$STEP" continue -n)
        assert_eq "$dry_cont" "Would run: git revert --continue" "revert continue dry-run"

        local dry_skip
        dry_skip=$(bash "$STEP" skip -n)
        assert_eq "$dry_skip" "Would run: git revert --skip" "revert skip dry-run"

        local dry_quit
        dry_quit=$(bash "$STEP" quit -n)
        assert_eq "$dry_quit" "Would run: git revert --quit" "revert quit dry-run"

        # Actually abort
        bash "$STEP" abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "revert abort succeeds"
    )
}

test_bisect_state() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        for i in 1 2 3; do
            echo "c$i" > file.txt
            git commit -q -am "c$i"
        done

        git bisect start
        git bisect bad HEAD
        git bisect good HEAD~2

        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "bisect" "detects bisect"

        set +e
        local cont_out cont_code
        cont_out=$(bash "$STEP" continue 2>&1)
        cont_code=$?
        assert_exit_code "$cont_code" 1 "bisect continue exits 1"
        assert_contains "$cont_out" "cannot be continued automatically" "bisect continue error"

        local dry_skip
        dry_skip=$(bash "$STEP" skip -n)
        assert_eq "$dry_skip" "Would run: git bisect skip" "bisect skip dry-run"

        local dry_quit
        dry_quit=$(bash "$STEP" quit -n)
        assert_eq "$dry_quit" "Would run: git bisect reset" "bisect quit dry-run"

        # Actually abort (reset)
        bash "$STEP" abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "bisect abort (reset) succeeds"
    )
}

test_am_state() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        echo "branch edit" > file.txt
        git commit -q -am "branch commit"
        local patch_file="$repo/test.patch"
        git format-patch -1 HEAD --stdout > "$patch_file"
        git reset -q --hard HEAD~1
        echo "conflict content" > file.txt
        git commit -q -am "conflicting commit"

        # Apply patch to cause conflict
        git am "$patch_file" 2>/dev/null || true

        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "am" "detects am"

        local dry_cont
        dry_cont=$(bash "$STEP" continue -n)
        assert_eq "$dry_cont" "Would run: git am --continue" "am continue dry-run"

        local dry_skip
        dry_skip=$(bash "$STEP" skip -n)
        assert_eq "$dry_skip" "Would run: git am --skip" "am skip dry-run"

        local dry_quit
        dry_quit=$(bash "$STEP" quit -n)
        assert_eq "$dry_quit" "Would run: git am --quit" "am quit dry-run"

        # Actually abort
        bash "$STEP" abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "am abort succeeds"
    )
}

test_worktree() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        echo "commit 1" > file.txt
        git commit -q -am "c1"
        local wt="$repo/worktree1"
        git worktree add -q -b wt-branch "$wt" HEAD

        cd "$wt"
        echo "wt edit" > file.txt
        git commit -q -am "wt commit"
        git checkout -q -b wt-feature HEAD~1
        echo "wt feature edit" > file.txt
        git commit -q -am "wt feature commit"

        # Rebase inside worktree
        git rebase wt-branch 2>/dev/null || true

        local status_out
        status_out=$(bash "$STEP" status)
        assert_contains "$status_out" "rebase" "worktree detects rebase"

        local dry_cont
        dry_cont=$(bash "$STEP" continue -n)
        assert_eq "$dry_cont" "Would run: git rebase --continue" "worktree continue dry-run"

        bash "$STEP" abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "worktree abort succeeds"
    )
}

test_git_alias() {
    local repo
    repo=$(make_temp_repo)
    (
        cd "$repo"
        # Test git continue alias when pointing to step script
        git config alias.continue "!bash \"$STEP\" continue"
        git config alias.abort "!bash \"$STEP\" abort"
        git config alias.skip "!bash \"$STEP\" skip"
        git config alias.quit "!bash \"$STEP\" quit"
        git config alias.step "!bash \"$STEP\""

        echo "main change" > file.txt
        git commit -q -am "main commit"
        git checkout -q -b feature HEAD~1
        echo "feature change" > file.txt
        git commit -q -am "feature commit"
        git rebase main 2>/dev/null || true

        local dry_cont
        dry_cont=$(git continue -n)
        assert_eq "$dry_cont" "Would run: git rebase --continue" "git continue alias works"

        local dry_skip
        dry_skip=$(git skip -n)
        assert_eq "$dry_skip" "Would run: git rebase --skip" "git skip alias works"

        local dry_quit
        dry_quit=$(git quit -n)
        assert_eq "$dry_quit" "Would run: git rebase --quit" "git quit alias works"

        local step_status
        step_status=$(git step)
        assert_contains "$step_status" "rebase" "git step status works"

        git abort
        local post_abort
        post_abort=$(bash "$STEP" status)
        assert_contains "$post_abort" "No operation in progress" "git abort alias works"
    )
}

# --- Main Runner ---
echo "Running git-step test suite..."
run_test test_help_and_usage
run_test test_clean_state
run_test test_unmerged_hint
run_test test_rebase_state
run_test test_merge_state
run_test test_cherry_pick_state
run_test test_revert_state
run_test test_bisect_state
run_test test_am_state
run_test test_worktree
run_test test_git_alias

echo ""
echo "Summary: $PASS_COUNT / $TEST_COUNT passed."
if [ "${#FAILURES[@]}" -gt 0 ]; then
    echo "Failures:"
    for f in "${FAILURES[@]}"; do
        echo "  - $f"
    done
    exit 1
fi
echo "All tests passed!"
