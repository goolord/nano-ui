#!/usr/bin/env python3
"""Fail when the `common warnings` stanzas of the packages' .cabal files differ.

Each package copies the same stanza, since a .cabal file cannot import one
from another file. This compares the copies byte for byte, indentation
included, and prints each one that differs from the most common copy.

Usage: python scripts/check-cabal-common.py [repo-root]
"""

import re
import sys
from collections import Counter
from pathlib import Path

STANZA = re.compile(r"^common warnings\n(?:[ \t]+\S.*\n)*", re.MULTILINE)


def main() -> int:
    root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parent.parent
    files = sorted((root / "packages").glob("*/*.cabal"))
    if not files:
        print(f"no .cabal files under {root / 'packages'}")
        return 1
    blocks = {}
    for path in files:
        text = path.read_text(encoding="utf-8").replace("\r\n", "\n")
        found = STANZA.findall(text)
        if len(found) != 1:
            print(f"{path.relative_to(root)}: {len(found)} 'common warnings' stanzas, expected 1")
            return 1
        blocks[path] = found[0]
    reference, _ = Counter(blocks.values()).most_common(1)[0]
    differing = [path for path, block in blocks.items() if block != reference]
    for path in differing:
        print(f"{path.relative_to(root)}: 'common warnings' differs from the other packages'")
        print(blocks[path], end="")
    if differing:
        print("expected:")
        print(reference, end="")
        return 1
    print(f"'common warnings' matches in {len(files)} .cabal files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
