Issue #324: reproducible frequentist BT refit evidence

Baseline: shmercer/pairwiseLLM 5a3fc17a77e31a88edacd58207537adc430a0f9a.
The six R files in tests/testthat/fixtures/bt-refits-324 are byte-for-byte
baseline source. helper-bt-refits-324.R sources them in a private environment
and rebinds the public dispatcher's environment to that scope. No reference
implementation calls the optimized preparation or information helpers.

Run from the repository root, with the optional analysis packages already
available. None of these commands installs into an existing package library,
uses a provider, or reads study outcomes:

  Rscript --vanilla scripts/profile-bt-refits.R /tmp/bt-profile
  PAIRWISELLM_TEST_BT_REFITS_LARGE=true Rscript --vanilla -e \
    'testthat::test_local(filter="0044", reporter="summary")'
  Rscript --vanilla scripts/benchmark-bt-refits-components.R /tmp/bt-components
  python3 scripts/benchmark-bt-refits.py /tmp/bt-workers
  Rscript --vanilla scripts/summarize-bt-refits.R \
    /tmp/bt-components /tmp/bt-workers /tmp/bt-summary
  Rscript --vanilla scripts/check-bt-conditioning.R /tmp/bt-conditioning.csv
  Rscript --vanilla scripts/check-bt-refits-coverage.R /tmp/bt-coverage
  R CMD build .
  R CMD check pairwiseLLM_1.6.0.tar.gz

results.txt explains the measured outcomes. fit-summary.csv and
worker-summary.csv summarize the raw fits.csv and bootstrap.csv;
allocations.csv reports separate allocation profiling. baseline-profile.csv
and public-profile.csv separate the six original computation stages.
worker-provenance.json records source hashes and hardware for the final
end-to-end measurements. Session and validation logs accompany the tables.
The coverage script needs a fresh output directory: covr can reuse trace
files left in an earlier installation. coverage.csv uses tally_coverage's
source-line expansion, not counts of instrumented expressions.

The worker driver installs the six original R files into a temporary baseline
package and the current source into a separate temporary prepared package.
Both parent and future workers load the selected installed namespace through
R_LIBS, avoiding startup differences from exporting sourced baseline functions.
It uses OUTPUT/baseline-library and OUTPUT/prepared-library, launches fresh R
processes, and uses the public bootstrap interface, including simulation,
validation, preparation, fitting, uncertainty checks, collection and worker
startup/shutdown. Parent package loading and a two-item engine warmup are
outside the bootstrap timer; process_seconds additionally records the entire
fresh R invocation. It compares 24 successful replicates in each of two runs
at 1,2,4,8,12 workers. Baseline/prepared order alternates between runs. All
scientific results must be identical to the first baseline serial run.
Twenty-four is divisible by every tested worker count, so all workers receive
whole waves of work (at least two fits per worker at the largest setting).
Local sockets are required for future multisession. This is not a tuning
recommendation for longer production runs or concurrent study workloads.

Short N=91 Firth runs showed regressions in some settings. A separate
sensitivity check uses 200 replicates, two runs and 1/12 workers, retaining
the same isolated installed libraries and exact scientific-result oracle:

  R_LIBS=/tmp/bt-workers/baseline-library Rscript --vanilla \
    scripts/benchmark-bt-firth-long.R /tmp/bt-firth-long baseline 1 1

Repeat for prepared-library/"prepared", workers 1 and 12, and runs 1 and 2;
run 1 orders baseline then prepared, run 2 prepared then baseline. Run the
baseline serial run 1 first to create its oracle. firth-long.csv contains
all eight cells, with source provenance in firth-long-provenance.json.
This additional check does not replace or filter the original 24-fit results.

The components benchmark reports six unprofiled per-fit times per estimator
and N (three fits in each of two runs), with GC before each timing. Graph
preparation is outside prepared-fit timing but INSIDE bootstrap timing.
Rprofmem allocations are measured separately from timing, twice per cell;
they are cumulative allocations, not peak resident memory. Cache size and
returned-object size are also reported. Python samples Linux /proc every
50 ms for peak summed process-tree RSS (including reparented workers in the
same session). Shared pages can be counted multiple times, and sub-50-ms
peaks can be missed. These RSS measures include R/package startup.

The baseline stage profile uses the actual original expression boundaries
for mapping/counts and design construction, then the original alpha kernel,
solver and uncertainty functions. The result-components row separately
measures theta/SE/SSR/comparison/list construction; it is not a claim to time
every branch in the complete result/provenance wrapper. Full-fit measurements
and Rprof stacks cover that wrapper. Stage profiling includes Rprofmem
overhead and must not be used as the final unprofiled speedup estimate.
Run 1 can include first-use loading/JIT costs. The initial candidate table
compares full baseline preparation with only the reusable indexed count
step; the final candidates.csv additionally isolates count aggregation.

Numerical contract fixed before evaluating equivalence:
- Exact: outcome counts, design entries and row order, scores, coefficients,
  solver iterations/deviance/convergence, adjustment provenance, validity
  flags, failure classes/reasons, and serial/parallel bootstrap summaries.
- Elementwise |optimized-reference| <= 1e-10 * (1+|reference|): SE,
  covariance, information/Hessian, adjustment score/gradient, Newton
  corrections and other floating-point diagnostics. This allows rounding
  from eliminating zero products and changing summation order while remaining
  three orders below the default 1e-7 stationarity thresholds. Tolerances are
  not tuned to observed differences.
- Graph families: complete, sparse, reversed orientation, unbalanced,
  near-separated, exactly balanced (zero scores), and separated; alpha is
  0, 1e-10 or 0.5. Small full-fit oracles run in CI. Explicit large testing
  adds N=57/91/229. Packaged example_writing_pairs provides an additional
  offline public-data fixture. No private or separate-study fixtures are used.
  A separate two-item alpha=1e5 oracle checks small nonzero scores without
  thresholding or classifying them as the exactly balanced solution.

Implementation choices:
- Ordinary lists local to a serial call or a bounded worker chunk cache item
  contrasts, observed-pair indexes,
  presentation directions, totals and designs. Alpha caches every unordered
  pair and both binary-outcome design blocks. Counts are always recomputed.
  For alpha=0 the unused duplicated binary block is omitted; active rows are
  selected with the original rule after counting each replicate's outcomes.
- Adaptive final fits share only item/all-pair structures; their realized
  comparison graph is rebuilt and validated each time. Custom estimators
  retain the existing path. Cache objects never enter bootstrap provenance.
- Workers construct dense designs locally instead of receiving them over
  sockets. Batches retain the original 2*workers result bound and reduction
  order; the cache is discarded with the chunk. There is no worker-global cache.
- Alpha information uses the reduced weighted graph Laplacian, exactly
  X' W X for the retained incidence design. Unpenalized information still
  determines reported uncertainty; the penalized Hessian still determines
  the independent Newton/stationarity check. When either already-computed
  reciprocal condition estimate is nonfinite or below 1e-5, the original
  dense BLAS cross-products are recomputed before numerical acceptance and
  uncertainty calculations. This conservative computational-path guard
  avoids amplified summation roundoff; it changes no scientific threshold.
  A tiny-alpha near-separated regression requires exact original diagnostics.
  It reproduced the Linux CI failure under a temporary OpenBLAS override;
  both reference BLAS and OpenBLAS pass after retaining the original products.
- Preserve the original merge() row-order distinction: complete graphs sort
  by pasted keys, incomplete graphs by numeric pair columns. This matters
  for bitwise solver equivalence once indices have multiple digits.
- Keep stats::glm.fit's weighted binary IWLS rows, starts and controls. Keep
  brglm2::brglmFit(type="AS_mean") and its formula wrapper, controls and raw
  model-object contract. No grouped-deviance shortcut, sparse solver,
  approximation, alternative penalty or engine fallback is introduced.

Remaining bottlenecks: the original dense QR/IWLS solvers dominate large-N
fits. Preparation caching alone cannot remove their cost. Firth's wrapper
and repeated covariance checks are deliberately retained; worker startup
and serialization can dominate short jobs. Dense cached alpha designs trade
retained per-worker memory for fewer repeated allocations. Production study
qualification and any repinning remain separate from this synthetic evidence.

Evidence text has trailing whitespace removed and CSV line endings normalized
to LF for repository checks; recorded values and log messages are unchanged.
