"""Count net maintained library lines, normalizing changed Haskell to Fourmolu's fixed point."""

import argparse
from pathlib import Path
import subprocess


def output(*command, text=None):
    return subprocess.run(command, input=text, capture_output=True, text=True, encoding="utf-8", check=True).stdout


def production(path):
    p = Path(path)
    return (len(p.parts) > 3 and p.parts[0] == "packages"
            and p.parts[1] != "nano-ui-demo" and p.parts[2] in {"lib", "cbits"}
            and p.suffix in {".hs", ".hsc", ".c", ".h"} and p.name != "RGFW.h")


def normalized(path, source):
    if not source or not path.endswith(".hs"):
        return source
    for _ in range(8):
        formatted = output("fourmolu", "--quiet", "--stdin-input-file", path, text=source)
        if formatted == source:
            return source
        source = formatted
    raise RuntimeError(f"Fourmolu did not reach a fixed point: {path}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline")
    args = parser.parse_args()
    before = set(filter(production, output("git", "ls-tree", "-r", "--name-only", args.baseline).splitlines()))
    after = {p for p in output("git", "ls-files", "--cached", "--others", "--exclude-standard").splitlines() if production(p) and Path(p).is_file()}
    raw_delta = normalized_delta = 0
    for path in sorted(before | after):
        old = output("git", "show", f"{args.baseline}:{path}") if path in before else ""
        new = Path(path).read_text(encoding="utf-8") if path in after else ""
        if old == new:
            continue
        raw = len(new.splitlines()) - len(old.splitlines())
        fixed = len(normalized(path, new).splitlines()) - len(normalized(path, old).splitlines())
        raw_delta += raw
        normalized_delta += fixed
        print(f"{raw:+5d} raw {fixed:+5d} normalized  {path}")
    print(f"Total: {raw_delta:+d} physical, {normalized_delta:+d} formatter-normalized library lines")


if __name__ == "__main__":
    main()
