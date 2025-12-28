# testthat organization

These tests are numbered to make the execution order and intent easy to scan.
The prefix groups tests by the corresponding `R/` module(s).

## 10xx — Data / example datasets
- Import helpers and built-in datasets.

## 20xx — Pair generation, core selection, allocation
- Pairing, reverse-pair handling, adaptive/core-link pair construction, allocation policies/presets.

## 30xx — Models, metrics, embeddings, judge diagnostics
- BT/Elo modeling helpers, drift/stopping metrics, embeddings utilities, judge diagnostics.

## 40xx — Rounds and runners
- Round state objects and end-to-end runners (adaptive, core linking, hybrid).

## 50xx — LLM backends + API integration
- Provider-specific and shared LLM batch/live plumbing.

## 70xx — Misc safety
- Seed / reproducibility safeguards.

Notes:
- File names are stable and map closely to the `R/*.R` sources.
