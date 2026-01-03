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

All test file numeric prefixes must be **unique**.

When a section would otherwise have multiple tests sharing a 2-digit prefix
(`24-...`, `31-...`, etc.), we use a **3-digit supplemental strategy** within
that section:

- Keep the section anchored (e.g., `24x`, `31x`, `46x`).
- Use the next available numbers (e.g., `240-...`, `241-...`; `310-...`, `311-...`).
- This keeps related tests adjacent while ensuring `devtools::test(filter = "<num>")`
  can target a single file by its unique numeric id.

We also use the same idea for focused branch-coverage files that we'd like to
keep near their section without renumbering everything (e.g., `245-...`,
`315-...`, `465-...`).

Notes:
- File names are stable and map closely to the `R/*.R` sources.


## 4-digit test IDs

For dense PR blocks, 4-digit IDs are allowed (e.g., `test-5300-...`) to keep related tests adjacent.
