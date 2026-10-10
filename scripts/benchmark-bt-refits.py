#!/usr/bin/env python3
"""Offline fresh-process bootstrap benchmark; no existing library is modified.

Run from repo root: python3 scripts/benchmark-bt-refits.py OUTPUT
Linux /proc sampling reports aggregate process-tree RSS, including workers.
RSS sums can count shared pages more than once; cumulative R allocations are
measured separately by benchmark-bt-refits-components.R.
"""
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time


def rss_tree(root):
    processes = {}
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit():
            continue
        try:
            fields = (entry / "stat").read_text().rsplit(")", 1)[1].split()
            # After comm: state, ppid, ... rss (field 24).
            processes[int(entry.name)] = (int(fields[1]), int(fields[21]) * os.sysconf("SC_PAGE_SIZE"), int(fields[3]))
        except (FileNotFoundError, ProcessLookupError, PermissionError):
            pass
    family = {root} | {pid for pid, (_, _, session) in processes.items() if session == root}
    while True:
        children = {pid for pid, (parent, _, _) in processes.items() if parent in family}
        extended = family | children
        if extended == family:
            break
        family = extended
    return sum(processes.get(pid, (0, 0))[1] for pid in family), len(family)


output = Path(sys.argv[1]).resolve()
output.mkdir(parents=True, exist_ok=True)
lib = output / "prepared-library"
baseline_lib = output / "baseline-library"
lib.mkdir(exist_ok=True)
baseline_lib.mkdir(exist_ok=True)
env = os.environ.copy()
sources = list(Path("R").glob("bt*.R")) + list(Path("scripts").glob("*bt-refits*"))
sources += list(Path("tests/testthat/fixtures/bt-refits-324").glob("*.R"))
sources += [Path("tests/testthat/helper-bt-refits-324.R"), Path("tests/testthat/test-0044-bt-prepared-refits.R")]
provenance = {
    "baseline_sha": "5a3fc17a77e31a88edacd58207537adc430a0f9a",
    "head_sha_at_measurement": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
    "source_sha256": {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in sources if p.is_file()},
    "cpu": subprocess.check_output(["lscpu"], text=True),
    "allowed_cpus": sorted(os.sched_getaffinity(0)),
    "rss": "50-ms samples, summed process-tree/session RSS; shared pages can be counted repeatedly",
    "replicates": 24, "repeated_runs": 2, "workers": [1, 2, 4, 8, 12], "seed": 324,
}
(output / "provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
with (output / "install.log").open("w") as log:
    subprocess.run(["R", "CMD", "INSTALL", "--library=" + str(lib), "."],
                   stdout=log, stderr=subprocess.STDOUT, env=env, check=True)
    # Only the six changed estimator/bootstrap files differ from baseline.
    # Install that original code as a namespace so future workers load it in
    # exactly the same way as the optimized package (no sourced-function proxy).
    baseline_source = output / "baseline-source"
    baseline_source.mkdir(exist_ok=True)
    for name in ("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md"):
        if Path(name).exists():
            shutil.copy2(name, baseline_source / name)
    for name in ("R", "man", "data", "inst"):
        shutil.copytree(name, baseline_source / name, dirs_exist_ok=True)
    for source in Path("tests/testthat/fixtures/bt-refits-324").glob("*.R"):
        shutil.copy2(source, baseline_source / "R" / source.name)
    subprocess.run(["R", "CMD", "INSTALL", "--library=" + str(baseline_lib), str(baseline_source)],
                   stdout=log, stderr=subprocess.STDOUT, env=env, check=True)
rows = []
for n in (57, 91, 229):
    for engine in ("alpha", "brglm2"):
        for workers in (1, 2, 4, 8, 12):
            for run in (1, 2):
                versions = ("baseline", "prepared") if run == 1 else ("prepared", "baseline")
                for version in versions:
                    env["R_LIBS"] = str(baseline_lib if version == "baseline" else lib)
                    label = f"{n}-{engine}-{workers}-{run}-{version}"
                    print(label, flush=True)
                    load_before = os.getloadavg()
                    with (output / (label + ".log")).open("w") as log:
                        process = subprocess.Popen(["Rscript", "--vanilla", "scripts/benchmark-bt-refits.R",
                            str(output), str(n), engine, version, str(workers), str(run)],
                            stdout=log, stderr=subprocess.STDOUT, env=env, start_new_session=True)
                        peak = count = 0
                        start = time.monotonic()
                        while process.poll() is None:
                            rss, processes = rss_tree(process.pid)
                            peak = max(peak, rss)
                            count = max(count, processes)
                            time.sleep(0.05)
                        if process.returncode:
                            raise RuntimeError(f"{label} failed; see its log")
                        with (output / "cell.csv").open() as result:
                            row = next(csv.DictReader(result))
                        row.update(peak_tree_rss_bytes=peak, max_processes=count,
                                   process_seconds=time.monotonic() - start,
                                   load_one_minute_before=load_before[0])
                        rows.append(row)
                        with (output / "bootstrap.csv").open("w", newline="") as result:
                            writer = csv.DictWriter(result, fieldnames=row.keys())
                            writer.writeheader()
                            writer.writerows(rows)
