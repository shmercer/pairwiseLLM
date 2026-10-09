# Issue #317: independent five-arm synthetic qualification

This is package engineering validation of #314–#316, not a production W-series
replay, scientific scoring exercise, calibration study, or estimate of study effects.
Current test/check/coverage results and observed CI status are recorded in
[PR #322](https://github.com/shmercer/pairwiseLLM/pull/322).

## Source and reproduction

- Base: `217fca553ae62296ea93d57b8883994f12cf627d`.
- Qualification implementation and benchmark source:
  `71ad2f163350bdcdddaa614f5316167d5398003e`.
- Package: pairwiseLLM 1.6.0; R 4.6.1 (2026-06-24), x86_64-pc-linux-gnu.
- This report and its CSV are evidence-only additions, excluded from package builds.
- Parsing the four changed package R files with `keep.source = FALSE` gives exactly
  the same executable expressions as the base. Changes there are Roxygen only;
  no statistical/API contract, dependency, or package version changed.

From an isolated checkout of the implementation commit, with test/documentation
Suggests already available, use one BLAS/OpenMP thread per R process:

```sh
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
Rscript --vanilla -e 'devtools::test(filter="^(0031|5116|5117|5118|9107)-", stop_on_failure=TRUE)'
Rscript --vanilla -e 'devtools::test(stop_on_failure=TRUE)'
Rscript --vanilla scripts/benchmark-predictive-tree.R /tmp/issue317-benchmark.csv
Rscript --vanilla -e 'devtools::document()'
git diff --check
```

R CMD check uses `rcmdcheck::rcmdcheck(args="--no-manual", build_args="--no-manual",
check_dir=<temporary directory>)`, including vignette builds and installed-package
tests. The local check library is temporary; the installed package used by ongoing
analyses is untouched. Real Stan/provider opt-ins remain disabled. Optional-engine
skips are reported separately from successful tests in the PR. The existing GitHub
Actions matrix covers Linux release/devel/oldrel-1, macOS release, Windows release,
coverage, and pkgdown. Source-vignette tests explicitly skip when source documents
are absent from an installed-package test context.

## Acceptance coverage

The 9107 integration tests use N=8 and N=9, dense and sparse connected selectable
graphs, recorded mixed orientations, fixed seeds, and ID-aligned heterogeneous SD.
A separate matched-prior test also covers scalar SD and two seeds, observing actual
BTL prior routing through a deterministic sampler stub; it makes no inference claim.

| Arm | BTL prior | Initial TrueSkill | Frozen graph |
| --- | --- | --- | --- |
| cold | default | cold | seeded shuffled |
| estimation | predictive | cold | seeded shuffled |
| legacy-graph coherent | predictive | predictive mean and SD | seeded shuffled |
| selection | default | predictive mean and SD | predictive tree |
| full | predictive | predictive mean and SD | predictive tree |

All five use Pollitt post-bootstrap. Independent scalar normal-probability and
reachability oracles check minimum-degree eligible focal items, optimal legal
partner distance to 1/3 or 2/3, the connected-tree boundary, and retained A/B/Y
orientation. Very small distance comparisons use an absolute 1e-14 tolerance to
allow floating-point complementation of recorded orientation near a target.

B is mean comparison exposures per essay. Each committed pair contributes two
exposures, so the prespecified total is `floor(B * N / 2)` with realized exposure
`2 * committed_pairs / N`. Bootstrap is included, never added to the total.

| N | B=0 | B=0.5 | B=1 | B=2 | Separate connectivity checkpoints |
| ---: | ---: | ---: | ---: | ---: | --- |
| 8 | 0 | 2 | 4 | 8 | 6, 7, 8 |
| 9 | 0 | 2 | 4 | 9 | 7, 8, 9 |

B=0 is prior-only. B=0.5 and B=1 precede connectivity and are not fully connected
CJ estimates. Invalid attempts/retries do not count as committed pairs. Tests
cross the boundary repeatedly, including failed attempts immediately before and
after tree completion, and inspect several subsequent Pollitt selections.

Selectable and held-out unordered edges are disjoint. Reversal audits intentionally
share unordered pairs with the primary layer but never supply reservoir observations.
Replacing/permuting held-out Y, reversal Y, and human scores leaves initialization,
priors, trees and selected traces unchanged. Changing selectable Y in a new reservoir
preserves initial ratings/prior/tree but changes judged updates and the outcome-bearing
reservoir identity; the changed judge cannot resume the original session.

Fresh-process tests cover all arms at 0, 3, N-1 and N committed pairs, including
invalid attempts. They retain exact committed history and updated ratings, prohibit
prediction/reinitialization and predictive tree rebuilding, and preserve the caller's
RNG. Historical shuffled-tree validation still reconstructs its seed-defined queue
to verify integrity; qualification preserves that existing behavior. Old sessions,
policy-switch refusal, tamper rejection without replacing saved artifacts, and
unchanged default summaries plus opt-in bootstrap audit fields are covered.

## Bounded tree performance

[Raw measurements](issue-317-predictive-tree-benchmark.csv): 24 runs, three per
case, seed 315. All structural and operation-count assertions passed, and trees
from fresh unprofiled workers were identical to separately profiled builds.
Hardware: AMD Ryzen 9 3900X, 12 cores/24 logical CPUs, approximately 126 GiB RAM.
R processes used one BLAS/OpenMP thread. The machine was shared with other work;
these are observed ranges, not isolated-hardware performance guarantees.

Each unprofiled build runs in a fresh R process. GNU `/usr/bin/time` reports
whole-worker maximum RSS, **including R/package startup, fixture construction,
and the build**. It is not incremental tree memory. Fixture creation and package
loading are excluded from the in-R build wall timer. Cold compilation/GC effects
can contribute to that timer. Cumulative allocation and its profiled wall time
are measured separately in the parent; neither is reported as peak RSS. The
script supports macOS `/usr/bin/time -l`; unsupported platforms explicitly record
unavailable RSS rather than substituting allocations. This run measured Linux.

| Graph | N | E | Build seconds, median [range] | Peak RSS MiB, max | Allocated MiB, median |
| --- | ---: | ---: | ---: | ---: | ---: |
| dense90 | 256 | 29376 | 0.084 [0.084, 0.089] | 205.05 | 9.91 |
| dense90 | 512 | 117734 | 0.392 [0.364, 0.412] | 263.26 | 36.51 |
| dense90 | 1164 | 609179 | 2.437 [1.990, 3.427] | 311.18 | 188.58 |
| dense99 | 256 | 32313 | 0.093 [0.089, 0.095] | 205.57 | 10.06 |
| dense99 | 512 | 129507 | 0.535 [0.502, 0.563] | 267.41 | 40.15 |
| dense99 | 1164 | 670097 | 4.289 [4.158, 4.339] | 309.39 | 207.40 |
| path | 1164 | 1163 | 0.033 [0.032, 0.035] | 183.75 | 0.70 |
| hub | 1164 | 1163 | 0.094 [0.088, 0.096] | 183.36 | 0.83 |

The builder scores E edges once, sorts once, and uses union by size/path compression
for component checks. Degree checks defer edges until caps double; there are at
most logarithmically many scans. The conservative bound is `O(E log E)` time and
`O(E + N)` space for a connected allowable graph. Diagnostics bound probability
evaluations by E, component checks by E, and candidate visits by
`E * max(1, ceiling(log2(N-1)))`. The hub fixture exercises ten cap relaxations;
path and dense cases finish without relaxation. No provider, Stan or BTL fits
are benchmarked. These timings cover tree construction, not large end-to-end
adaptive replay, persistence, or model estimation.

## Limits and integration boundary

- Synthetic SD values exercise mapping/alignment, not uncertainty calibration.
  Calibration remains an upstream, unverified responsibility. Mapping raw BTL SD
  to TrueSkill assumes comparable latent scales and does not equate posteriors.
- The caller must exclude held-out/reversal evidence; the package cannot infer a
  study's evidence partition from arbitrary supplied observations.
- Sampler stubs validate prior delivery only. Scientific scoring, private data,
  model training/LOPO, production replay, study repinning and releases are excluded.
- No material executable package R lines changed, so there is no newly changed
  runtime-code scope for a 95% line-coverage gate. Actual package coverage is
  measured by CI and reported in the PR rather than inferred from passing tests.
- No scientific discrepancy was resolved by adding a fallback. Merge and release
  remain maintainer decisions.
