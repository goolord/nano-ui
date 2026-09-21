"""Compare already-built Cabal trees without rebuilding either candidate.

Example: python scripts/profile/compare-builds.py dist-newstyle /tmp/candidate
         --suite render --output /tmp/render.json
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import statistics
import subprocess


def run(command):
    result = subprocess.run(command, capture_output=True, text=True, encoding="utf-8", errors="replace")
    if result.returncode:
        raise RuntimeError(f"{command}\n{result.stdout}\n{result.stderr}")
    return result.stdout, result.stderr


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline")
    parser.add_argument("candidate")
    parser.add_argument("--suite", choices=["core", "sdl", "render", "atlas", "events"], required=True)
    parser.add_argument("--runs", type=int, default=7)
    parser.add_argument("--executables", action="store_true", help="compare executable paths instead of Cabal build directories")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    target = {"core": "exe:nano-ui-profile", "sdl": "exe:nano-ui-sdl-profile", "render": "test:nano-ui-render-test", "atlas": "test:nano-ui-render-test", "events": "test:nano-ui-rgfw-bindings-test"}[args.suite]
    executables = {}
    for label, directory in [("baseline", args.baseline), ("candidate", args.candidate)]:
        if args.executables:
            executables[label] = directory
        else:
            stdout, _ = run(["cabal", "list-bin", f"--builddir={directory}", target])
            executables[label] = stdout.strip().splitlines()[-1]
    scenes = ["widgets", "canvas", "canvas-keyed", "textarea", "svg"] if args.suite == "core" else [args.suite]
    records = []
    for iteration in range(args.runs):
        for scene in scenes:
            for label in (["baseline", "candidate"] if iteration % 2 == 0 else ["candidate", "baseline"]):
                command = [executables[label]]
                command += [scene, "+RTS", "-t", "--machine-readable"] if args.suite == "core" else ({"render": ["--bench"], "atlas": ["--atlas-bench"], "events": ["--bench"]}.get(args.suite, []))
                stdout, stderr = run(command)
                metrics = {}
                if args.suite == "core":
                    stats = dict(re.findall(r'\("([^"]+)", "([^"]+)"\)', stderr))
                    metrics[scene] = {"seconds": float(stats["mut_wall_seconds"]), "bytes": float(stats["allocated_bytes"])}
                elif args.suite == "events":
                    for name, nanoseconds, allocated in re.findall(r"^(.+?): ([\d.]+) ns/read \| ([\d.]+) B/read", stdout, re.M):
                        metrics[name] = {"ns": float(nanoseconds), "bytes": float(allocated)}
                else:
                    for name, milliseconds, allocated, unit in re.findall(r"^(.+?)\s*:\s*([\d.]+) ms/frame\s*\|\s*([\d.]+) (KB alloc|B)/frame", stdout, re.M):
                        metrics[name.strip()] = {"ms": float(milliseconds), "bytes": float(allocated) * (1024 if unit == "KB alloc" else 1)}
                if not metrics:
                    raise RuntimeError(f"No metrics parsed: {stdout}\n{stderr}")
                records.append(dict(iteration=iteration, version=label, scene=scene, metrics=metrics, stdout=stdout, stderr=stderr))
        print(f"pair {iteration + 1}/{args.runs}", flush=True)
    summary = {}
    for record in records:
        for scene, metrics in record["metrics"].items():
            for metric in metrics:
                key = f"{scene}/{metric}"
                if key in summary:
                    continue
                samples = {label: [r["metrics"][scene][metric] for r in records if r["version"] == label and scene in r["metrics"]] for label in executables}
                medians = {label: statistics.median(values) for label, values in samples.items()}
                delta = 100 * (medians["candidate"] / medians["baseline"] - 1) if medians["baseline"] else None
                summary[key] = dict(**medians, percent=delta, samples=samples)
                print(f"{key}: {medians['baseline']:.6f} -> {medians['candidate']:.6f} ({delta:+.2f}%)" if delta is not None else f"{key}: {medians}")
    hashes = {label: hashlib.sha256(Path(path).read_bytes()).hexdigest() for label, path in executables.items()}
    args.output.write_text(json.dumps(dict(executables=executables, sha256=hashes, summary=summary, records=records), indent=2), encoding="utf-8")


if __name__ == "__main__":
    main()
