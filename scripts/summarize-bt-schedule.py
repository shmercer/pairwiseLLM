#!/usr/bin/env python3
"""Summarize bounded native cells without rerunning or changing their evidence."""
import argparse
import csv
import json
from pathlib import Path
import re


def summarize_profile(path):
    lines = path.read_text().splitlines()
    interval = int(re.search(r"sample.interval=(\d+)", lines[0]).group(1)) / 1e6
    groups = [
        ("native_refits", r"default_btl_fit_fn$"),
        ("final_estimation", r"\.bt_bootstrap_refit$"),
        ("prepared_design", r"\.bt_bootstrap_prepare$"),
        ("outcome_generation", r"\.bt_bootstrap_(simulate|judge)$"),
        ("clone_hash_serialization", r"(^|:)(serialize|unserialize|\.warm_start_prior_hash|digest)$"),
        ("selection", r"(select_next_pair|\.adaptive_warm_start_selection)$"),
        ("trueskill", r"(update_trueskill_state|trueskill_update|\.adaptive_trueskill[^\"]*)$"),
        ("validation", r"(validate|check_judge|resolve_config)"),
        ("state_logs_checkpoints", r"(controller|stop_boundary|phase_a|persist_session|append_.*log|apply_step_update)"),
    ]
    counts = {}
    for line in lines[1:]:
        stack = re.findall(r'"([^\"]+)"', line)
        group = next((name for name, pattern in groups
                      if any(re.search(pattern, function) for function in stack)), "other")
        counts[group] = counts.get(group, 0) + 1
    total = sum(counts.values())
    return [dict(category=name, samples=count, seconds=count * interval, fraction=count / total)
            for name, count in sorted(counts.items())]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    rows = []
    profiles = []
    for path in sorted(args.input.rglob("measurements.json")):
        for row in json.loads(path.read_text()):
            rows.append(dict(run=str(path.parent.relative_to(args.input)), **row))
    for path in sorted(args.input.rglob("Rprof.out")):
        if not path.stat().st_size or not (path.parent / "cell.csv").exists():
            continue
        for row in summarize_profile(path):
            profiles.append(dict(run=str(path.parent.relative_to(args.input)), **row))
    for name, data in [("measurements.csv", rows), ("profile-components.csv", profiles)]:
        if data:
            fields = list(dict.fromkeys(key for row in data for key in row))
            with (args.output / name).open("w", newline="") as out:
                writer = csv.DictWriter(out, fieldnames=fields)
                writer.writeheader()
                writer.writerows(data)


if __name__ == "__main__":
    main()
