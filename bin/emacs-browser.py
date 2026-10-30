#!/usr/bin/env python3
"""Index files present in one checkout and serve fuzzy results to Consult."""
import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import time

CACHE = Path.home() / ".emacs.d/browser-cache"
RG = shutil.which("rg") or "rg"
FZF = shutil.which("fzf") or str(Path.home() / ".fzf/bin/fzf")


def cache_name(root):
    return "tree-" + hashlib.sha256(os.fsencode(root)).hexdigest()


def fresh(root, target):
    if not target.is_file():
        return False
    marker = root / ".git"
    gitdir = root / marker.read_text().strip().removeprefix("gitdir: ") if marker.is_file() else marker
    stamp = target.stat().st_mtime
    # A status check may rewrite Git's index without changing any paths.
    markers = (gitdir / "HEAD", gitdir / "logs/HEAD", gitdir / "info/sparse-checkout")
    return time.time() - stamp < 86400 and all(
        path.stat().st_mtime <= stamp for path in markers if path.exists()
    )


def index(root, ensure=False):
    """Publish a complete index atomically; keep the old index on any failure."""
    CACHE.mkdir(parents=True, exist_ok=True)
    name = cache_name(root)
    target = CACHE / (name + ".files0")
    if ensure and fresh(root, target):
        return
    with (CACHE / (name + ".lock")).open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if ensure and fresh(root, target):
            return
        temporary = target.with_suffix(".pending")
        start = time.monotonic()
        try:
            with temporary.open("wb") as output:
                subprocess.run(
                    [RG, "--files", "--hidden", "--null", "--glob", "!.git",
                     "--glob", "!node_modules", "--glob", "!target",
                     "--glob", "!.venv", "--glob", "!venv", "--glob", "!bazel-*"],
                    cwd=root, stdout=output, check=True, timeout=180,
                )
            count = 0
            with temporary.open("rb") as source:
                for block in iter(lambda: source.read(1024 * 1024), b""):
                    count += block.count(b"\0")
            if not count:
                raise RuntimeError("Refusing to publish an empty index")
            temporary.replace(target)
            metadata = {"root": str(root), "files": count,
                        "updated_at": time.strftime("%Y-%m-%dT%H:%M:%S%z"),
                        "seconds": round(time.monotonic() - start, 3)}
            metadata_path = CACHE / (name + ".json")
            metadata_temp = metadata_path.with_suffix(".pending")
            metadata_temp.write_text(json.dumps(metadata, indent=2) + "\n")
            metadata_temp.replace(metadata_path)
            if not ensure:
                print(json.dumps(metadata))
        finally:
            temporary.unlink(missing_ok=True)


def find(root, terms):
    """Keep ranking/filtering outside Emacs and cap the result set."""
    query = " ".join(terms).strip()
    if not query:
        return
    target = CACHE / (cache_name(root) + ".files0")
    # The picker starts an independent refresh. Serve the last complete list immediately.
    if not target.is_file():
        index(root, ensure=True)
    env = dict(os.environ, FZF_DEFAULT_OPTS="", FZF_DEFAULT_OPTS_FILE="/dev/null")
    with target.open("rb") as source:
        process = subprocess.Popen(
            [FZF, "--read0", "--scheme=path", "--filter=" + query], stdin=source,
            stdout=subprocess.PIPE, env=env,
        )
        # Consult cancels obsolete searches. Never leave a fuzzy worker behind.
        try:
            count = 0
            for line in process.stdout:
                if (root / os.fsdecode(line.rstrip(b"\n"))).is_file():
                    sys.stdout.buffer.write(line)
                    count += 1
                    if count == 500:
                        break
            sys.stdout.buffer.flush()
        except BrokenPipeError:
            pass
        finally:
            process.stdout.close()
            if process.poll() is None:
                process.terminate()
            process.wait()


def recently_used(roots, days):
    """Read Git's worktree registrations and the editor's use markers."""
    cutoff = time.time() - days * 86400
    found = {}
    for root in roots:
        registry = root / ".git/worktrees"
        if not registry.is_dir():
            continue
        for entry in registry.iterdir():
            try:
                marker = Path((entry / "gitdir").read_text().removesuffix("\n"))
                if not marker.is_absolute():
                    marker = entry / marker
                tree = marker.parent.resolve()
                used = (CACHE / (cache_name(tree) + ".used")).stat().st_mtime
                if used < cutoff or marker.name != ".git" or not marker.is_file():
                    continue
                # Ignore removed/replaced checkouts and stale registrations.
                pointer = marker.read_text().removesuffix("\n")
                if not pointer.startswith("gitdir: ") or not (tree / pointer[8:]).samefile(entry):
                    continue
                found[tree] = used
            except (FileNotFoundError, NotADirectoryError):
                continue
    return sorted(found, key=found.get, reverse=True)


def main():
    global CACHE
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache-dir", type=Path, default=CACHE)
    commands = parser.add_subparsers(dest="action", required=True)
    for action in ("index", "ensure", "find"):
        command = commands.add_parser(action)
        command.add_argument("root", type=Path)
        if action == "find":
            command.add_argument("query", nargs=argparse.REMAINDER)
    warm = commands.add_parser("warm-recent", help="index recently browsed linked worktrees")
    warm.add_argument("roots", type=Path, nargs="+", help="canonical Git checkouts")
    warm.add_argument("--days", type=int, default=7)
    args = parser.parse_args()
    CACHE = args.cache_dir.expanduser()
    if args.action == "warm-recent":
        trees = recently_used(args.roots, args.days)
        failed = False
        for tree in trees:
            try:
                index(tree)
            except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
                print(f"{tree}: {exc}", file=sys.stderr)
                failed = True
        print(f"Recent worktrees selected: {len(trees)}", file=sys.stderr)
        return int(failed)
    root = args.root.resolve()
    if not (root / ".git").exists():
        parser.error("Expected a Git checkout root")
    if args.action in ("index", "ensure"):
        index(root, ensure=args.action == "ensure")
    else:
        # Also interrupt a first-use scan cleanly if Consult cancels its query.
        def cancel(_signal, _frame):
            raise SystemExit(0)
        signal.signal(signal.SIGTERM, cancel)
        find(root, args.query)


if __name__ == "__main__":
    sys.exit(main())
