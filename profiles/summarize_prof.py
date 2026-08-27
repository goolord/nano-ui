import json
from collections import defaultdict
from pathlib import Path

prof = Path(__file__).with_name("nano-ui-profile.prof")
with prof.open() as f:
    d = json.load(f)

cc_by_id = {c["id"]: c for c in d["cost_centres"]}
by_label: dict[str, dict[str, float]] = defaultdict(lambda: {"ticks": 0, "alloc": 0})
skip_modules = {"IDLE", "PROFILING", "SYSTEM", "GC"}


def walk(node: dict) -> None:
    cid = node.get("id")
    ticks = node.get("ticks") or 0
    alloc = node.get("alloc") or 0
    if cid is not None and cid in cc_by_id:
        c = cc_by_id[cid]
        if c["module"] not in skip_modules and c["label"] != "DONT_CARE":
            key = f"{c['module']}.{c['label']}"
            by_label[key]["ticks"] += ticks
            by_label[key]["alloc"] += alloc
    for ch in node.get("children") or []:
        walk(ch)


roots = d["profile"] if isinstance(d["profile"], list) else [d["profile"]]
for root in roots:
    walk(root)

total_ticks = d["total_ticks"]
print(f"total_time: {d['total_time']}s  total_ticks: {total_ticks}")
print(f"{'ticks':>8} {'%':>6}  {'alloc MB':>10}  cost centre")
for key, v in sorted(by_label.items(), key=lambda kv: kv[1]["ticks"], reverse=True)[:30]:
    pct = 100.0 * v["ticks"] / total_ticks if total_ticks else 0.0
    mb = v["alloc"] / 1e6
    print(f"{int(v['ticks']):>8} {pct:>5.1f}%  {mb:>10.1f}  {key}")
