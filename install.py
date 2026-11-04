#!/usr/bin/env python3
import os
import platform
import shutil
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

# --- Environment Setup ---
IS_MSYS = "MSYSTEM" in os.environ or sys.platform.startswith(("msys", "cygwin"))
IS_WIN = platform.system() == "Windows" and not IS_MSYS
UNIX = IS_MSYS or platform.system() in ("Linux", "Darwin")
LINUX = platform.system() == "Linux"

DOT_HOME = Path(os.environ.get("DOT_HOME", Path(__file__).resolve().parent))
HOME = Path.home()
XDG_CONFIG = Path(os.environ.get("XDG_CONFIG_HOME", HOME / ".config"))
APPDATA = Path(os.environ.get("APPDATA", HOME))

PLAN = list()

# --- UI Helpers ---
def _msg(c : int, p: str):
    if not sys.stdout.isatty():
        def inner(m):
            print(f"{p} {m}")
    else:
        def inner(m):
            print(f"\033[1;{c}m{p}\033[0m {m}")
    return inner

RED = 31
GREEN = 32

info = _msg(34, "=>")
success = _msg(32, "✓")
warn = _msg(33, "⚠")
err = _msg(31, "✗")


def ask(prompt, default="s"):
    try:
        ans = input(f"{prompt} ").strip().lower()
        return ans[0] if ans else default
    except (KeyboardInterrupt, EOFError):
        print("\nAborted by user.")
        sys.exit(130)


def step(name, *pairs, to=None, post=None):
    assert len(pairs) % 2 == 0, "link() expects pairs of src and trg"

    for i in range(0, len(pairs), 2):
        s = pairs[i]
        t = pairs[i + 1]
        if isinstance(s, str):
            pairs[i] = DOT_HOME / s
        if isinstance(t, str):
            pairs[i + 1] = Path(t).expanduser()
    PLAN.append((name, pairs, post))

# --- Configuration ---
def plan():
    """Generator yielding (src, trg, [post_action]) tuples."""

    # Emacs
    yield "emacs", "~/.emacs.d"

    if UNIX:
        yield "zsh/.zshenv", "~/.zshenv"
        yield "zsh", XDG_CONFIG / "zsh"
    # SSH
    def apply_perms():
        p = DOT_HOME / "ssh/config"
        if not IS_WIN and p.exists():
            p.chmod(0o640)

    yield "ssh/config", "~/.ssh/config", apply_perms

    # Profiles
    if not IS_WIN:
        yield ".profile", "~/.profile"

    # Xorg
    if LINUX:
        for f in [".xprofile", ".Xresources", ".xinitrc"]:
            yield f"xorg/{f}", f"~/{f}"

    # GnuPG
    gpg_base = APPDATA / "gnupg" if IS_WIN else HOME / ".gnupg"
    yield "gnupg/gpg-agent.conf", gpg_base / "gpg-agent.conf"
    yield "gnupg/gpg.conf", gpg_base / "gpg.conf"

    # XDG Configs
    cfgs = ["doom", "git", "ripgrep", "sh"]
    if not IS_WIN:
        cfgs += ["i3", "i3status", "dunst", "rofi", "termite", "kitty", "autorandr"]
    for c in cfgs:
        yield c, XDG_CONFIG / c


    # AI Agents
    yield "agents/AGENTS.md", "~/.agents/AGENTS.md"
    yield "agents/AGENTS.md", "~/.omp/agent/AGENTS.md"
    yield "agents/AGENTS.md", "~/.claude/CLAUDE.md"
    yield "agents/AGENTS.md", "~/.gemini/AGENTS.md"
    yield "agents/AGENTS.md", "~/.gemini/GEMINI.md"
    yield "agents/AGENTS.md", "~/.codex/AGENTS.md"
    yield "agents/AGENTS.md", XDG_CONFIG / "opencode/AGENTS.md"
    # Bin scripts
    if UNIX:
        bin_dir = DOT_HOME / "bin"
        if bin_dir.is_dir():
            for f in bin_dir.iterdir():
                if f.is_file():
                    yield f"bin/{f.name}", f"~/.local/bin/{f.name}"


# --- Engine ---
def execute():
    info("Planning dotfiles installation...")
    print(
        f"  Source: {DOT_HOME}\n  Target: {HOME}\n  System: {platform.system()} ({platform.release()})\n"
    )

    tasks = []
    for item in plan():
        src = DOT_HOME / item[0]
        if not src.exists():
            continue  # Gracefully skip if the repo doesn't have the file

        trg = Path(str(item[1])).expanduser()
        post = item[2] if len(item) > 2 else None
        tasks.append((src, trg, post))

    if not tasks:
        success("No tasks planned.")
        return

    info("Planned tasks:")
    for i, (src, trg, _) in enumerate(tasks, 1):
        print(f"  [{i}] {src.relative_to(DOT_HOME)} ==> {trg}")

    print()
    if ask("Proceed with installation? (y/N)", "n") != "y":
        print("Aborted by user.")
        sys.exit(0)

    print()
    info("Executing planned tasks...")
    failed = False

    def run_post(p):
        if not p:
            return
        try:
            p()
        except Exception as e:
            warn(f"Post action failed: {e}")

    for src, trg, post in tasks:
        src = src.resolve()

        try:
            if trg.is_symlink() and trg.resolve() == src:
                print(f"Already linked: {src.name} ==> {trg}")
                run_post(post)
                continue
            if trg.is_file() and src.is_file() and trg.read_bytes() == src.read_bytes():
                print(f"Identical file exists: {src.name} ==> {trg}")
                run_post(post)
                continue
        except OSError:
            pass

        if trg.exists() or trg.is_symlink():
            ans = ask(f"Conflict: {trg} already exists. (r)eplace/(s)kip/(b)ackup?", "s")
            if ans == "r":
                (shutil.rmtree if trg.is_dir() and not trg.is_symlink() else trg.unlink)(trg)
            elif ans == "b":
                bkp = trg.with_name(f"{trg.name}.{datetime.now():%Y-%m-%d!%H-%M-%S}.bak")
                print(f"Backing up {trg} as {bkp}")
                shutil.move(str(trg), str(bkp))
            else:
                failed = True
                continue

        trg.parent.mkdir(parents=True, exist_ok=True)
        print(f"Linking {src.name} ==> {trg}")
        try:
            trg.symlink_to(src, target_is_directory=src.is_dir())
            run_post(post)
        except OSError as e:
            err(f"Failed to create symlink: {e}")
            failed = True

    print()
    if failed:
        err("Some operations failed or were skipped. Please inspect the log.")
        sys.exit(1)
    success("Install completed successfully.")


if __name__ == "__main__":
    execute()
