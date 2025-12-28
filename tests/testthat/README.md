# testthat organization

Tests are numbered to make execution order and intent easy to scan. The numeric
prefix groups tests by the corresponding `R/` module(s).

## 10–19 — Data / example datasets
- Import helpers and built-in datasets.

## 20–29 — Pair generation, core selection, allocation
- Pairing, reverse-pair handling, adaptive/core-link pair construction, allocation policies/presets.

## 30–39 — Models, metrics, embeddings, judge diagnostics
- BT/Elo modeling helpers, drift/stopping metrics, embeddings utilities, judge diagnostics.

## 40–49 — Rounds and runners
- Round state objects and end-to-end runners (adaptive, core linking, hybrid).

## 50–59 — LLM backends + API integration
- Provider-specific and shared LLM batch/live plumbing.

## 70–79 — Misc safety
- Seed / reproducibility safeguards.

### Supplemental / branch-coverage tests
Occasionally we add focused tests that exercise hard-to-hit branches without
reorganizing the primary sequence. These use 3-digit numbers within a group
(e.g., `245-...`, `315-...`, `465-...`) so they remain near the relevant section
and avoid renumbering existing files.

Notes:
- File names are stable and map closely to the `R/*.R` sources.
