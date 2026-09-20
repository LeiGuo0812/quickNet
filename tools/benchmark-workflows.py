#!/usr/bin/env python3
"""Linux benchmark with sampled RSS across R and its worker processes.

Run from the package root. Each case uses three fresh processes. RSS sums shared
pages repeatedly, so it is a process-tree RSS measure, not unique physical RAM.
"""
import csv
import argparse
import json
import os
from pathlib import Path
import platform
import subprocess
import time

out = Path("../output/audit/performance")
out.mkdir(parents=True, exist_ok=True)
env = dict(os.environ, OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS="1", MKL_NUM_THREADS="1")

def process_tree_rss(root):
    entries = {}
    for path in Path("/proc").iterdir():
        if not path.name.isdigit():
            continue
        try:
            info = dict(line.split(":", 1) for line in (path / "status").read_text().splitlines() if ":" in line)
            entries[int(path.name)] = (int(info["PPid"]), int(info.get("VmRSS", "0 kB").split()[0]))
        except (OSError, ValueError, KeyError):
            continue
    children = {root}
    while True:
        added = {pid for pid, (parent, _) in entries.items() if parent in children}
        if added <= children:
            break
        children |= added
    # PSOCK workers can be reparented after launch; the isolated session also
    # retains those workers after their intermediate shell exits.
    for pid in entries:
        try:
            if os.getsid(pid) == root:
                children.add(pid)
        except ProcessLookupError:
            pass
    return sum(entries[pid][1] for pid in children if pid in entries)

parser = argparse.ArgumentParser()
parser.add_argument("--method", choices=("all", "nira", "symperturb"), default="all")
parser.add_argument("--resume", action="store_true")
config = parser.parse_args()
rows = list(csv.DictReader((out / "measurements.csv").open())) if config.resume and (out / "measurements.csv").exists() else []
cases = [("nira", size, cores) for size in ("small", "medium") for cores in (1, 2)]
cases += [("symperturb", size, 1) for size in ("small", "medium")]
for method, size, cores in cases:
    if config.method != "all" and config.method != method:
        continue
    for repetition in range(1, 4):
        if any(row["method"] == method and row["size"] == size and int(row["cores"]) == cores and
               int(row["repetition"]) == repetition for row in rows):
            continue
        name = f"{method}-{size}-{cores}-{repetition}"
        table = out / f"{name}.csv"
        started = time.monotonic()
        with (out / f"{name}.log").open("w") as log:
            process = subprocess.Popen(["Rscript", "--vanilla", "tools/benchmark-workflows.R", method, size, str(cores), str(table)],
                                       env=env, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
            peak = 0
            while process.poll() is None:
                if time.monotonic() - started > 180:
                    os.killpg(process.pid, 15)
                    process.wait(timeout=10)
                    raise RuntimeError(f"Benchmark timed out: {name}; incomplete measurement excluded")
                peak = max(peak, process_tree_rss(process.pid))
                time.sleep(0.1)
            if process.returncode:
                raise RuntimeError(f"Benchmark failed: {name}; see log")
        row = next(csv.DictReader(table.open()))
        row.update(repetition=repetition, process_seconds=time.monotonic()-started, peak_tree_rss_kib=peak)
        rows.append(row)
        with (out / "measurements.csv").open("w", newline="") as file:
            writer = csv.DictWriter(file, fieldnames=rows[0].keys())
            writer.writeheader()
            writer.writerows(rows)
        print(name, row["elapsed_seconds"], peak, flush=True)
(out / "machine.json").write_text(json.dumps({"platform": platform.platform(), "cpu_count": os.cpu_count(),
    "cpuinfo": Path("/proc/cpuinfo").read_text().split("\n\n")[0],
    "threads": {name: env[name] for name in ("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS")},
    "rss_sample_seconds": 0.1, "shared_pages_counted_per_process": True}, indent=2))
