#!/usr/bin/env python3
"""Sequential, bounded synthetic benchmark sections for issue #325.

Requires fresh isolated libraries from prepare-bt-schedule-benchmarks.py.
Run only one worker section at a time; reserve its complete native-chain budget.
All four total chains, seeds, warmup and sampling remain unchanged. No production
study files, providers, dependency installation or active libraries are touched.
"""
import argparse
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("section", choices=("serial", "strategies", "firth", "workers", "chain-parity", "large-chain-parity"))
    parser.add_argument("--libraries", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--versions", default="post328,optimized,study")
    parser.add_argument("--configurations", default="1x4,2x4,3x4,4x2,8x1,12x1")
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    configurations = [tuple(map(int, item.split("x"))) for item in args.configurations.split(",")]
    allowed = {(1, 4), (2, 4), (3, 4), (4, 2), (8, 1), (12, 1)}
    if not set(configurations) <= allowed:
        parser.error("use approved worker/concurrent-chain combinations")
    versions = args.versions.split(",")
    if not set(versions) <= {"post328", "optimized", "study"}:
        parser.error("unknown isolated library label")
    all_policies = "random,trueskill_p50,trueskill_pollitt,hybrid"
    jobs = []
    if args.section == "workers":
        for w, pc in configurations:
            for n, b, engine in [(91, 8, "brglm2"), (229, 4, "alpha")]:
                for version in versions:
                    jobs.append((f"{version}-w{w}-pc{pc}-n{n}", version,
                        ["--n", str(n), "--B", str(b), "--engines", engine,
                         "--workers", str(w), "--parallel-chains", str(pc),
                         "--replicates", str(max(4, 2 * w)), "--conditions", "2",
                         "--keep", "summary", "--execution",
                         "pool" if version == "optimized" and w > 1 else "managed"]))
    elif args.section == "large-chain-parity":
        for w, pc in configurations:
            for n, b, engine in [(91, 8, "brglm2"), (229, 4, "alpha")]:
                for version in versions:
                    jobs.append((f"{version}-w{w}-pc{pc}-n{n}", version,
                        ["--n", str(n), "--B", str(b), "--engines", engine,
                         "--workers", str(w), "--parallel-chains", str(pc),
                         "--replicates", "2", "--keep", "full", "--execution",
                         "pool" if version == "optimized" and w > 1 else "managed"]))
    elif args.section == "chain-parity":
        for w, pc in configurations:
            for version in versions:
                jobs.append((f"{version}-w{w}-pc{pc}", version,
                    ["--n", "57", "--B", "4", "--engines", "alpha,brglm2",
                     "--strategies", all_policies, "--workers", str(w),
                     "--parallel-chains", str(pc), "--replicates", "2",
                     "--execution", "pool" if version == "optimized" and w > 1 else "managed"]))
    else:
        for version in versions:
            if args.section == "serial":
                jobs.append((version, version,
                    ["--n", "57,91,229", "--B", "4,8,16", "--engines", "alpha"]))
            elif args.section == "strategies":
                jobs.append((version, version,
                    ["--n", "57", "--B", "4", "--engines", "alpha,brglm2",
                     "--strategies", all_policies]))
            else:
                for n, b in [(57, 4), (91, 8), (229, 16)]:
                    jobs.append((f"{version}-n{n}", version,
                        ["--n", str(n), "--B", str(b), "--engines", "brglm2"]))
    for label, version, options in jobs:
        output = args.output / label
        if output.exists():
            parser.error(f"refusing to overwrite {output}; choose a fresh section output")
        command = ["python3", "scripts/benchmark-bt-schedule.py",
            "--library", str((args.libraries / (version + "-library")).resolve()),
            "--version", version, "--output", str(output.resolve())] + options
        print("Starting", label, flush=True)
        with (args.output / (label + ".log")).open("w") as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True)
        print("Completed", label, flush=True)


if __name__ == "__main__":
    main()
