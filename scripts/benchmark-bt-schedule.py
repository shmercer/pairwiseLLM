#!/usr/bin/env python3
"""Bounded native adaptive bootstrap cells; Linux process-tree CPU/RSS evidence.

Install each frozen source into a separate library first. Example:
python3 scripts/benchmark-bt-schedule.py --library /tmp/baseline-library \
  --version post328 --output /tmp/baseline-profile --profile --n 57 --B 4

The full serial profile grid is --n 57,91,229 --B 4,8,16
--strategies random,trueskill_p50,trueskill_pollitt,hybrid --engines alpha,brglm2.
Cells are sequential. Worker/chain configurations are separate invocations.
No providers or private study artifacts are read. No installation is changed.
"""
import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import time


def mem_available():
    for line in Path("/proc/meminfo").read_text().splitlines():
        if line.startswith("MemAvailable:"):
            return int(line.split()[1]) * 1024
    raise RuntimeError("MemAvailable is required for bounded benchmarking")


def physical_slots():
    cores = set()
    for cpu in os.sched_getaffinity(0):
        topology = Path(f"/sys/devices/system/cpu/cpu{cpu}/topology")
        cores.add(((topology / "physical_package_id").read_text().strip(),
                   (topology / "core_id").read_text().strip()))
    physical = len(cores)
    quota = Path("/sys/fs/cgroup/cpu.max")
    available = float(physical)
    if quota.exists():
        amount, period = quota.read_text().split()
        if amount != "max":
            available = min(available, int(amount) / int(period))
    return physical, max(1, int(available * .8))


def sample_tree(root):
    processes = {}
    page_size = os.sysconf("SC_PAGE_SIZE")
    ticks = os.sysconf("SC_CLK_TCK")
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit():
            continue
        try:
            fields = (entry / "stat").read_text().rsplit(")", 1)[1].split()
            processes[int(entry.name)] = dict(parent=int(fields[1]), session=int(fields[3]),
                rss=int(fields[21]) * page_size, cpu=(int(fields[11]) + int(fields[12])) / ticks,
                start=fields[19])
        except (FileNotFoundError, ProcessLookupError, PermissionError):
            pass
    family = {root} | {pid for pid, p in processes.items() if p["session"] == root}
    while True:
        extended = family | {pid for pid, p in processes.items() if p["parent"] in family}
        if extended == family:
            break
        family = extended
    selected = {pid: processes[pid] for pid in family if pid in processes}
    return sum(p["rss"] for p in selected.values()), selected


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--library", required=True, type=Path)
    parser.add_argument("--version", required=True)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--n", default="57")
    parser.add_argument("--B", default="4")
    parser.add_argument("--strategies", default="trueskill_p50")
    parser.add_argument("--engines", default="alpha")
    parser.add_argument("--workers", type=int, default=1)
    parser.add_argument("--parallel-chains", type=int, default=4)
    parser.add_argument("--replicates", type=int, default=2)
    parser.add_argument("--conditions", type=int, default=1,
                        help="Split the total replicates across independent seeded calls to measure pool reuse")
    parser.add_argument("--profile", action="store_true")
    parser.add_argument("--execution", choices=("managed", "pool"), default="managed")
    parser.add_argument("--keep", choices=("summary", "theta", "full"), default="full")
    parser.add_argument("--max-seconds", type=float, default=3600)
    args = parser.parse_args()
    if not 2 <= args.replicates <= 24:
        parser.error("bounded cells require 2..24 replicates")
    if args.conditions < 1 or args.replicates % args.conditions or args.replicates / args.conditions < 2:
        parser.error("each condition must have an equal integer number of at least two replicates")
    if args.workers < 1 or args.parallel_chains not in (1, 2, 4):
        parser.error("positive workers and 1/2/4 parallel chains required")
    args.output.mkdir(parents=True, exist_ok=True)
    if (args.output / "provenance.json").exists():
        parser.error("choose a fresh output directory; existing evidence is never overwritten")
    physical, slots = physical_slots()
    sources = [Path(__file__), Path(__file__).with_name("benchmark-bt-schedule-cell.R")]
    metadata = dict(arguments={k: str(v) if isinstance(v, Path) else v for k, v in vars(args).items()},
        package_library=str(args.library.resolve()), physical_cores=physical, reserved_budget=slots,
        affinity=sorted(os.sched_getaffinity(0)), mem_available_start=mem_available(),
        git_head=subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
        source_sha256={str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in sources},
        sampling="100ms summed parent/worker/CmdStan RSS and per-PID user+system CPU; "
                 "shared pages can be counted repeatedly; short-lived processes/peaks can be missed",
        native_configuration=dict(refit_pairs_target="N", chains=4, iter_warmup=1000,
            iter_sampling=1000, threads_per_chain=1, core_fraction=.8, model_variant="btl_e_b",
            native_max_rhat=1.01),
        cpu=subprocess.check_output(["lscpu"], text=True))
    (args.output / "provenance.json").write_text(json.dumps(metadata, indent=2) + "\n")
    env = os.environ.copy()
    env.update(R_LIBS=str(args.library.resolve()), BT_SCHEDULE_KEEP=args.keep,
               BT_SCHEDULE_CONDITIONS=str(args.conditions),
               OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS="1", MKL_NUM_THREADS="1", MAKEFLAGS="-j1")
    records = []
    for n in map(int, args.n.split(",")):
        for b in map(int, args.B.split(",")):
            for strategy in args.strategies.split(","):
                for engine in args.engines.split(","):
                    label = f"n{n}-b{b}-{strategy}-{engine}"
                    directory = args.output / label
                    directory.mkdir(exist_ok=True)
                    record = dict(cell=label, status="pending", reason="", load_average_before=os.getloadavg())
                    if args.workers * args.parallel_chains > slots:
                        record.update(status="skipped", reason="physical_cpu_budget")
                    # Retained alpha cache alone is a lower bound, never an RSS prediction.
                    elif n == 229 and engine == "alpha" and args.workers * 139 * 2**20 > .7 * mem_available():
                        record.update(status="skipped", reason="prepared_cache_memory_lower_bound")
                    else:
                        print(label, flush=True)
                        command = ["Rscript", "--vanilla", "scripts/benchmark-bt-schedule-cell.R",
                            str(directory.resolve()), str(n), str(b), strategy, engine, args.version,
                            str(args.workers), str(args.replicates), str(args.parallel_chains),
                            "profile" if args.profile else "timing", args.execution]
                        start = time.monotonic()
                        cpu_last = {}
                        cpu_seconds = peak = max_processes = 0
                        workload_cpu = workload_peak = 0
                        with (directory / "run.log").open("w") as log:
                            process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT,
                                env=env, start_new_session=True)
                            while process.poll() is None:
                                rss, family = sample_tree(process.pid)
                                peak = max(peak, rss)
                                max_processes = max(max_processes, len(family))
                                in_workload = ((directory / "workload.started").exists()
                                               and not (directory / "workload.finished").exists())
                                if in_workload:
                                    workload_peak = max(workload_peak, rss)
                                for pid, p in family.items():
                                    key = (pid, p["start"])
                                    delta = max(0, p["cpu"] - cpu_last.get(key, 0))
                                    cpu_seconds += delta
                                    if in_workload:
                                        workload_cpu += delta
                                    cpu_last[key] = p["cpu"]
                                reason = ""
                                if rss > .7 * mem_available():
                                    reason = "live_memory_budget"
                                elif time.monotonic() - start > args.max_seconds:
                                    reason = "cell_time_budget"
                                if reason:
                                    os.killpg(process.pid, signal.SIGTERM)
                                    try:
                                        process.wait(timeout=5)
                                    except subprocess.TimeoutExpired:
                                        os.killpg(process.pid, signal.SIGKILL)
                                        process.wait()
                                    record.update(status="skipped", reason=reason)
                                    break
                                time.sleep(.1)
                        wall = time.monotonic() - start
                        record.update(process_seconds=wall, sampled_tree_cpu_seconds=cpu_seconds,
                            average_cpu_cores=cpu_seconds / wall, physical_cpu_utilization=cpu_seconds / wall / physical,
                            peak_tree_rss_bytes=peak, max_processes=max_processes,
                            workload_tree_cpu_seconds=workload_cpu, workload_peak_tree_rss_bytes=workload_peak,
                            load_average_after=os.getloadavg())
                        if record["status"] == "pending":
                            record.update(status="complete" if process.returncode == 0 else "error",
                                          reason="" if process.returncode == 0 else f"exit_{process.returncode}")
                        if (directory / "cell.csv").exists():
                            with (directory / "cell.csv").open() as source:
                                record.update(next(csv.DictReader(source)))
                            record.update(workload_average_cpu_cores=workload_cpu / float(record["elapsed_seconds"]),
                                workload_physical_cpu_utilization=workload_cpu / float(record["elapsed_seconds"]) / physical)
                    records.append(record)
                    (args.output / "measurements.json").write_text(json.dumps(records, indent=2) + "\n")
                    print(record, flush=True)
    if any(row["status"] == "error" for row in records):
        raise SystemExit("One or more cells failed; inspect measurements.json and per-cell logs")


if __name__ == "__main__":
    main()
