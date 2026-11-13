#!/usr/bin/env bash
# Test suite for git-land. Run: bash test-git-land.sh
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
GIT_LAND="$SCRIPT_DIR/../git-land"
export PATH="$SCRIPT_DIR/..:$PATH"

PASS=0
FAIL=0

test_pass() {
    PASS=$((PASS + 1))
    echo "  PASS: $1"
}

test_fail() {
    FAIL=$((FAIL + 1))
    echo "  FAIL: $1 - $2" >&2
}

setup_repo() {
    local dir
    dir="$(mktemp -d)"
    (
        cd "$dir"
        git init -q -b main
        git config user.email "test@example.com"
        git config user.name "Tester"
        echo "base" > file.txt
        git add file.txt
        git commit -q -m "initial commit"
    )
    echo "$dir"
}

# -------------------------------------------------------------
echo "1. Basic clean land (deletes branch)"
repo="$(setup_repo)"
(
    cd "$repo"
    git checkout -q -b feature
    echo "feature work" >> file.txt
    git commit -q -am "feat 1"
    git checkout -q main
    echo "main work" > main.txt
    git add main.txt
    git commit -q -m "main 1"

    "$GIT_LAND" feature >/dev/null

    [ "$(git branch --show-current)" = "main" ] || exit 1
    git show-ref --verify --quiet refs/heads/feature && exit 2
    git log --oneline | grep -q "feat 1" || exit 3
)
if [ $? -eq 0 ]; then test_pass "basic clean land"; else test_fail "basic clean land" "branch not deleted or commits missing"; fi
rm -rf "$repo"

# -------------------------------------------------------------
echo "2. Clean land with -k / --keep (resets branch to prior head)"
repo="$(setup_repo)"
(
    cd "$repo"
    git checkout -q -b feature
    echo "feature work" >> file.txt
    git commit -q -am "feat 1"
    orig_feat_sha="$(git rev-parse feature)"

    git checkout -q main
    echo "main work" > main.txt
    git add main.txt
    git commit -q -m "main 1"

    "$GIT_LAND" -k feature >/dev/null

    [ "$(git branch --show-current)" = "main" ] || exit 1
    git show-ref --verify --quiet refs/heads/feature || exit 2
    [ "$(git rev-parse feature)" = "$orig_feat_sha" ] || exit 3
    git log --oneline main | grep -q "feat 1" || exit 4
)
if [ $? -eq 0 ]; then test_pass "-k flag preserves branch at prior head"; else test_fail "-k flag" "failed to keep or reset branch"; fi
rm -rf "$repo"

# -------------------------------------------------------------
echo "3. Clean land with --keep after branch name"
repo="$(setup_repo)"
(
    cd "$repo"
    git checkout -q -b feature
    echo "feature work" >> file.txt
    git commit -q -am "feat 1"
    orig_feat_sha="$(git rev-parse feature)"

    git checkout -q main
    echo "main work" > main.txt
    git add main.txt
    git commit -q -m "main 1"

    "$GIT_LAND" feature --keep >/dev/null

    [ "$(git rev-parse feature)" = "$orig_feat_sha" ] || exit 1
)
if [ $? -eq 0 ]; then test_pass "--keep after branch name"; else test_fail "--keep after branch" "failed"; fi
rm -rf "$repo"

# -------------------------------------------------------------
echo "4. Dry-run (-n / --dry-run)"
repo="$(setup_repo)"
(
    cd "$repo"
    git checkout -q -b feature
    echo "feature" >> file.txt
    git commit -q -am "feat 1"
    orig_main_sha="$(git rev-parse main)"
    git checkout -q main

    out="$("$GIT_LAND" -n feature)"
    echo "$out" | grep -q "Would execute:" || exit 1
    [ "$(git rev-parse main)" = "$orig_main_sha" ] || exit 2
    git show-ref --verify --quiet refs/heads/feature || exit 3
)
if [ $? -eq 0 ]; then test_pass "dry run does not mutate repository"; else test_fail "dry run" "mutated state"; fi
rm -rf "$repo"

# -------------------------------------------------------------
echo "5. Precondition errors"
repo="$(setup_repo)"
(
    cd "$repo"
    # No args
    "$GIT_LAND" 2>/dev/null && exit 1
    # Same branch
    "$GIT_LAND" main 2>/dev/null && exit 2
    # Nonexistent branch
    "$GIT_LAND" non-existent 2>/dev/null && exit 3
    # Dirty tree
    git checkout -q -b feature
    git checkout -q main
    echo "dirty" >> file.txt
    "$GIT_LAND" feature 2>/dev/null && exit 4
    git checkout -q -- file.txt
    # Detached head
    git checkout -q --detach HEAD
    "$GIT_LAND" feature 2>/dev/null && exit 5
    exit 0
)
if [ $? -eq 0 ]; then test_pass "precondition errors caught"; else test_fail "preconditions" "did not catch invalid state"; fi
rm -rf "$repo"

# -------------------------------------------------------------
echo "6. Conflict resolution lifecycle (continue)"
repo="$(setup_repo)"
(
    cd "$repo"
    git checkout -q -b feature
    echo "conflict from feature" > file.txt
    git commit -q -am "feat conflict"

    git checkout -q main
    echo "conflict from main" > file.txt
    git commit -q -am "main conflict"

    # Must stop with conflict
    "$GIT_LAND" feature >/dev/null 2>&1 && exit 1

    # Resolve conflict
    echo "resolved content" > file.txt
    git add file.txt

    # Resume with git land
    "$GIT_LAND" >/dev/null

    [ "$(git branch --show-current)" = "main" ] || exit 2
    git show-ref --verify --quiet refs/heads/feature && exit 3
    git log -1 --pretty=%s | grep -q "feat conflict" || exit 4
)
if [ $? -eq 0 ]; then test_pass "conflict recovery with 'git land'"; else test_fail "conflict recovery" "failed"; fi
rm -rf "$repo"

# -------------------------------------------------------------
echo "7. Conflict resolution with -k / --keep preserved across resume"
repo="$(setup_repo)"
(
    cd "$repo"
    git checkout -q -b feature
    echo "conflict from feature" > file.txt
    git commit -q -am "feat conflict"
    orig_feat_sha="$(git rev-parse feature)"

    git checkout -q main
    echo "conflict from main" > file.txt
    git commit -q -am "main conflict"

    # Start with -k
    "$GIT_LAND" -k feature >/dev/null 2>&1 && exit 1

    # Resolve conflict
    echo "resolved content" > file.txt
    git add file.txt

    # Resume with git land
    "$GIT_LAND" >/dev/null

    [ "$(git branch --show-current)" = "main" ] || exit 2
    git show-ref --verify --quiet refs/heads/feature || exit 3
    [ "$(git rev-parse feature)" = "$orig_feat_sha" ] || exit 4
    git log -1 --pretty=%s | grep -q "feat conflict" || exit 5
)
if [ $? -eq 0 ]; then test_pass "conflict recovery with -k preserves branch"; else test_fail "conflict with -k" "failed"; fi
rm -rf "$repo"

# -------------------------------------------------------------
echo "8. Conflict abort lifecycle"
repo="$(setup_repo)"
(
    cd "$repo"
    orig_main_sha="$(git rev-parse main)"
    git checkout -q -b feature
    echo "conflict from feature" > file.txt
    git commit -q -am "feat conflict"
    orig_feat_sha="$(git rev-parse feature)"

    git checkout -q main
    echo "conflict from main" > file.txt
    git commit -q -am "main conflict"
    orig_main_sha="$(git rev-parse main)"

    "$GIT_LAND" feature >/dev/null 2>&1 && exit 1

    # Abort
    "$GIT_LAND" abort >/dev/null

    [ "$(git branch --show-current)" = "main" ] || exit 2
    [ "$(git rev-parse main)" = "$orig_main_sha" ] || exit 3
    [ "$(git rev-parse feature)" = "$orig_feat_sha" ] || exit 4
)
if [ $? -eq 0 ]; then test_pass "abort returns cleanly to target branch"; else test_fail "abort" "state corrupted"; fi
rm -rf "$repo"
# -------------------------------------------------------------
echo "9. Worktree support"
repo="$(setup_repo)"
(
    cd "$repo"
    git branch -q wt-base
    wt_dir="$(mktemp -d)"
    git worktree add -q "$wt_dir" wt-base
    (
        cd "$wt_dir"
        git checkout -q -b wt-feature
        echo "wt feature" > wt.txt
        git add wt.txt
        git commit -q -m "wt feat"
        git checkout -q wt-base
        echo "wt base work" > wt-base.txt
        git add wt-base.txt
        git commit -q -m "wt base commit"

        "$GIT_LAND" -k wt-feature >/dev/null

        [ "$(git branch --show-current)" = "wt-base" ] || exit 1
        git log --oneline | grep -q "wt feat" || exit 2
    )
    rm -rf "$wt_dir"
    git worktree prune
)
if [ $? -eq 0 ]; then test_pass "worktree support"; else test_fail "worktree" "failed in linked worktree"; fi
rm -rf "$repo"
# -------------------------------------------------------------
echo "10. git-step integration (status, continue, abort)"
repo="$(setup_repo)"
(
    cd "$repo"
    git_step="$SCRIPT_DIR/../git-step"
    orig_main_sha="$(git rev-parse main)"
    git checkout -q -b feature
    echo "feat conflict" > file.txt
    git commit -q -am "feat 1"
    orig_feat_sha="$(git rev-parse feature)"

    git checkout -q main
    echo "main conflict" > file.txt
    git commit -q -am "main 1"

    # Start land and hit conflict
    "$GIT_LAND" -k feature >/dev/null 2>&1 && exit 1

    # 1. git-step status should report 'land'
    status_out="$("$git_step" status)"
    echo "$status_out" | grep -q "Operation in progress: land" || exit 2
    echo "$status_out" | grep -q "landing feature onto main" || exit 3

    # 2. git-step abort should cancel land and return to main
    "$git_step" abort >/dev/null
    [ "$(git branch --show-current)" = "main" ] || exit 4
    [ "$(git rev-parse feature)" = "$orig_feat_sha" ] || exit 5

    # 3. Start again, resolve conflict, git-step continue should finish land
    "$GIT_LAND" -k feature >/dev/null 2>&1 && exit 6
    echo "resolved" > file.txt
    git add file.txt
    "$git_step" continue >/dev/null

    [ "$(git branch --show-current)" = "main" ] || exit 7
    [ "$(git rev-parse feature)" = "$orig_feat_sha" ] || exit 8
    git log -1 --pretty=%s | grep -q "feat 1" || exit 9
)
if [ $? -eq 0 ]; then test_pass "git-step integration"; else test_fail "git-step" "failed git-step integration"; fi
rm -rf "$repo"

echo ""
echo "Summary: $PASS passed, $FAIL failed."
[ "$FAIL" -eq 0 ]
