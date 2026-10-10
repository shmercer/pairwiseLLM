#!/usr/bin/env python3
"""Export compact final measurements, provenance and paired timing tables.

Only the named final experiment directories are included; exploratory/v1 cells
and memo diagnostic probes are deliberately excluded from timing comparisons.
Native audit/equivalence CSVs are exported separately by the R validation tools.
"""
import argparse
import csv
import json
from pathlib import Path


def write_csv(path, rows):
    if not rows:
        return
    fields = list(dict.fromkeys(key for row in rows for key in row))
    with path.open("w", newline="") as out:
        writer = csv.DictWriter(out, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    families = {"timing-post328": "serial-grid", "timing-optimized": "serial-grid",
        "serial-matrix": "serial-grid", "strategies-matrix": "strategies",
        "firth-matrix": "firth", "worker-matrix": "workers",
        "chain-parity": "qualification", "chain-parity-w12": "qualification",
        "profile-optimized": "optimized-profile", "large-chain-parity": "qualification"}
    rows, conditions, provenance = [], [], {}
    for folder, family in families.items():
        for path in sorted((args.input / folder).rglob("measurements.json")):
            run = str(path.parent.relative_to(args.input))
            info = json.loads((path.parent / "provenance.json").read_text())
            provenance[run] = info
            for record in json.loads(path.read_text()):
                row = dict(family=family, run=run, **record)
                rows.append(row)
                cell = path.parent / record["cell"]
                if record["status"] == "complete":
                    code = json.loads((cell / "loaded-code.json").read_text())
                    expected_namespace = str(Path(info["package_library"]) / "pairwiseLLM")
                    if code["namespace_path"] != expected_namespace:
                        raise RuntimeError(f"wrong loaded namespace: {cell}")
                    row["namespace_path"] = code["namespace_path"]
                    provenance[run].setdefault("loaded_code_by_cell", {})[record["cell"]] = code
                    with (cell / "conditions.csv").open() as source:
                        conditions.extend(dict(family=family, run=run, cell=record["cell"], **r)
                                          for r in csv.DictReader(source))
    write_csv(args.output / "measurements.csv", rows)
    write_csv(args.output / "condition-times.csv", conditions)
    (args.output / "benchmark-provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
    # Pair complete calls with identical workload sizes/settings. Pool startup
    # is included in the primary rate comparison. Qualification profiles and
    # diagnostic probes are not throughput estimates for occupied worker pools.
    groups = {}
    keys = ("family", "n", "B", "strategy", "engine", "workers", "parallel_chains", "n_rep", "conditions")
    for row in rows:
        if row["status"] != "complete" or row["family"] in ("qualification", "optimized-profile"):
            continue
        key = tuple(row[k] for k in keys)
        version = row["implementation"].split("-")[0]
        groups.setdefault(key, {})[version] = row
    comparisons = []
    for key, versions in groups.items():
        if "optimized" not in versions:
            continue
        optimized = versions["optimized"]
        for baseline in ("post328", "study"):
            if baseline not in versions:
                continue
            original = versions[baseline]
            before = float(original["elapsed_seconds"]) + float(original["pool_start_seconds"])
            after = float(optimized["elapsed_seconds"]) + float(optimized["pool_start_seconds"])
            if original["n_success"] != optimized["n_success"]:
                raise RuntimeError(f"unexplained success-count difference for {key}")
            comparisons.append(dict(zip(keys, key), baseline=baseline,
                baseline_seconds_including_pool_start=before,
                optimized_seconds_including_pool_start=after, elapsed_speedup=before / after,
                n_success=optimized["n_success"], n_failed=optimized["n_failed"],
                baseline_successful_replicates_per_second=float(original["n_success"]) / before,
                optimized_successful_replicates_per_second=float(optimized["n_success"]) / after,
                baseline_workload_peak_tree_rss_mib=float(original["workload_peak_tree_rss_bytes"]) / 2**20,
                optimized_workload_peak_tree_rss_mib=float(optimized["workload_peak_tree_rss_bytes"]) / 2**20))
    write_csv(args.output / "paired-comparisons.csv", comparisons)
    print(f"Exported {len(rows)} measured cells and {len(comparisons)} matched comparisons")


if __name__ == "__main__":
    main()
