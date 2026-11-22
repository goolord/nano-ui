"""Fail when a profile scene allocates more than its committed baseline.

Each scene of nano-ui-profile runs a fixed number of frames, so the bytes it
allocates are the same from run to run on one GHC version. This script runs
every scene in the baseline file, reads the total from the RTS summary, and
fails if any scene grew by more than the tolerance. Timings are not gated:
they vary with the machine. Compare those with compare-builds.py.

Check:   python scripts/profile/alloc-gate.py
Update:  python scripts/profile/alloc-gate.py --update
Another binary: python scripts/profile/alloc-gate.py --exe path/to/nano-ui-profile
"""

import argparse
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path

BASELINE = Path(__file__).with_name("alloc-baseline.json")


def profile_exe(explicit):
    if explicit:
        return explicit
    out = subprocess.run(
        ["cabal", "list-bin", "exe:nano-ui-profile"],
        capture_output=True, text=True, check=True,
    ).stdout
    return out.strip().splitlines()[-1]


def allocated(exe, scene):
    with tempfile.TemporaryDirectory() as tmp:
        stats = Path(tmp) / "stats.txt"
        args = [exe, *scene.split(), "+RTS", f"-s{stats}", "-RTS"]
        subprocess.run(args, check=True, capture_output=True, cwd=tmp)
        text = stats.read_text(encoding="utf-8", errors="replace")
    match = re.search(r"([\d,]+) bytes allocated in the heap", text)
    if not match:
        raise RuntimeError(f"no allocation total for scene {scene!r}:\n{text}")
    return int(match.group(1).replace(",", ""))


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--exe", help="nano-ui-profile binary (default: cabal list-bin)")
    parser.add_argument("--update", action="store_true", help="rewrite the baseline from this run")
    args = parser.parse_args()

    baseline = json.loads(BASELINE.read_text(encoding="utf-8"))
    tolerance = baseline["tolerance"]
    exe = profile_exe(args.exe)
    failed = False
    for scene, expected in baseline["scenes"].items():
        got = allocated(exe, scene)
        change = (got - expected) / expected if expected else 0.0
        status = "ok"
        if change > tolerance:
            status = "REGRESSION"
            failed = True
        print(f"{scene:20} {got:>16,} bytes  ({change:+.1%} vs {expected:,})  {status}")
        if args.update:
            baseline["scenes"][scene] = got
    if args.update:
        BASELINE.write_text(json.dumps(baseline, indent=2) + "\n", encoding="utf-8")
        print(f"wrote {BASELINE}")
        return 0
    if failed:
        print(f"allocation grew by more than {tolerance:.0%}; if intended, rerun with --update")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
