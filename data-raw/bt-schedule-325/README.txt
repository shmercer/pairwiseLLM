Issue #325: bounded, provider-free adaptive bootstrap qualification

References
  post-#328: 7999c15c6c50b0d8de670ff49702648630d8235b
  historical pairwiseLLM 1.6.0 study: 4c4c01453077f90e987040c6a3c3d3cd711c6f21
  implementation measured: ad083c992c0fc548ebefb36fd553e07d21b8ed8d
Later commits add/fix tests, reproducibility scripts and evidence; implementation
source hashes in installations.json and loaded-code.json identify actual code.
The historical comparison runs the frozen package source with the same current
R/dependencies/CmdStan and synthetic inputs as the candidate. It does not replay
private study outcomes or claim to recreate an older dependency environment.
Neither baseline library is the active study installation. All installations,
model compilation, and synthetic artifacts used a separate /tmp location.

Scientific protocol, fixed before comparing results
  Synthetic theta = seq(-1, 1, length.out=N), named item001, item002, ... .
  Cold shuffled-connected initialization, seed 325. B4/B8/B16 mean 2N/4N/8N
  committed judgments, respectively. Strategies: random, trueskill_p50,
  trueskill_pollitt, hybrid. Final estimators: alpha=.5 and brglm2 Firth.
  Native model btl_e_b; refit_pairs_target=N; four TOTAL chains with original
  canonical seeds/chain IDs; warmup=1000, sampling=1000, threads_per_chain=1,
  core_fraction=.8. Native Rhat threshold stays 1.01. No refit is omitted.
  Primary resource settings: 1/2/3 simultaneous trajectories, parallel_chains=4.
  Approved resource experiments: 4 trajectories x 2 concurrent chains and
  8/12 trajectories x 1 concurrent chain. These still run four total chains.
  Exact comparisons for initialization, presentations, outcomes, full scientific
  states, per-attempt RNG, seeds, refit cadence, stops, posterior matrices,
  diagnostics, estimates, and failure inventory. No new numerical tolerance.
  The existing #328 information/covariance bound 1e-10*(1+abs(reference)) applies
  only to its algebraically reordered uncertainty matrices, not schedule/score
  or native posterior equality. The #325 implementation changes no arithmetic.
  Normalize only incidental phase-A wall-clock timestamps, function/environment
  identity, and the enumerated execution-resource metadata when concurrent-chain
  counts differ. Historical schemas predate predictive/bootstrap metadata: check
  established scientific fields and all shared log/meta columns, and enumerate
  schema differences. The validator does not silently loosen equality.

Observed execution-metadata difference during concurrent-chain qualification
  The runtime prepare_context_by_set digest includes the full latest refit row,
  including five recorded resource settings. It therefore changes when chain
  concurrency changes. A separate observer-only native probe captured the exact
  hash inputs and confirmed this cause. For comparisons with different concurrent
  chain counts, the validator reconstructs every memo input, requires its hash to
  equal the actual stored digest, and requires the two input objects to be exact
  after removing only the already enumerated resource settings. Only then does
  it replace that digest with a canonical hash for the full-state comparison.
  Raw hashes and canonical input hashes are retained in memo-resource-proofs.csv.
  Comparisons at the same resource settings still require exact original memo
  digests. This is a documented metadata exception, not a numerical tolerance
  change or a scientific-state omission. scripts/probe-bt-schedule-memo.R can
  reproduce the independent input capture using the cell runner's arguments.

Reproduction (from the repository root; dependencies already provisioned)
  Measured environment: Ryzen 9 5900XT, 16 physical/32 logical cores, 128 GiB
  RAM, Linux, R 4.6.1, cmdstanr 0.9.0, CmdStan 2.38.0. See session.txt and
  provenance JSON for versions. Use Python 3.12+ for archive extraction.
  python3 scripts/prepare-bt-schedule-benchmarks.py /tmp/325-libs --candidate HEAD
  python3 scripts/run-bt-schedule-matrix.py serial --libraries /tmp/325-libs \
    --output /tmp/325-evidence/serial-matrix
  python3 scripts/run-bt-schedule-matrix.py strategies --libraries /tmp/325-libs \
    --output /tmp/325-evidence/strategies-matrix
  python3 scripts/run-bt-schedule-matrix.py firth --libraries /tmp/325-libs \
    --output /tmp/325-evidence/firth-matrix
  python3 scripts/run-bt-schedule-matrix.py workers --libraries /tmp/325-libs \
    --versions post328,optimized --output /tmp/325-evidence/worker-matrix
  python3 scripts/run-bt-schedule-matrix.py chain-parity --libraries /tmp/325-libs \
    --versions optimized --configurations 2x4,4x2,8x1,12x1 --output /tmp/325-evidence/chain-parity
  Rscript --vanilla scripts/validate-bt-schedule.R /tmp/325-evidence/strategies-matrix/post328 \
    /tmp/325-evidence/strategies-matrix/optimized /tmp/325-exact
  Rscript --vanilla scripts/validate-bt-schedule.R /tmp/325-evidence/strategies-matrix/study \
    /tmp/325-evidence/strategies-matrix/optimized /tmp/325-historical-exact historical
  python3 scripts/run-bt-schedule-matrix.py large-chain-parity --libraries /tmp/325-libs \
    --versions optimized --configurations 4x2,12x1 \
    --output /tmp/325-evidence/large-chain-parity
  # Compare large N91/Firth to firth-matrix/post328-n91 and large N229/alpha
  # to serial-matrix/post328, retaining the full-state/resource-preimage checks.
  # Compare each chain-parity directory to strategies-matrix/post328 above.
  # Compare matching post328/optimized worker directories to each other.
  python3 scripts/summarize-bt-schedule.py /tmp/325-evidence/worker-matrix /tmp/325-summary

Individual cells and the complete baseline profile grid
  python3 scripts/benchmark-bt-schedule.py --library /tmp/325-libs/post328-library \
    --version post328 --output /tmp/325-evidence/profile-baseline --n 57,91,229 --B 4,8,16 --profile
  R_LIBS=/tmp/325-libs/post328-library Rscript --vanilla \
    scripts/profile-bt-schedule-components.R /tmp/325-evidence/profile-baseline /tmp/325-components
  R_LIBS=/tmp/325-libs/post328-library Rscript --vanilla \
    scripts/profile-bt-schedule-transport.R /tmp/325-evidence/profile-baseline /tmp/325-transport-w2 2
  Rscript --vanilla scripts/check-bt-schedule-coverage.R /tmp/325-coverage
  python3 scripts/benchmark-bt-schedule.py --library /tmp/325-libs/optimized-library \
    --version optimized --output /tmp/325-evidence/profile-optimized \
    --n 57,91,229 --B 4 --profile
  python3 scripts/summarize-bt-schedule.py /tmp/325-evidence/profile-baseline \
    /tmp/325-profile-summary
  python3 scripts/export-bt-schedule-evidence.py /tmp/325-evidence /tmp/325-export
  Rscript --vanilla scripts/export-bt-schedule-audits.R /tmp/325-evidence /tmp/325-export
  # The exporters use the experiment-family directory names shown above.
  # Run validations after their complete candidate directory finishes; incomplete
  # grids, missing independent baselines and unexplained differences are errors.

Interpretation and limits
  Each section is bounded; this is not a production study run or a 200-replicate
  matrix. Serial native qualification retains full trajectories. Worker timing
  uses default summary retention across two independent seeded calls; the caller
  pool persists between calls. Matched versions use the same seeds and number of
  replicates at each worker configuration. Replicate count grows with workers
  (max(4,2*workers), at most 24) to exercise every worker, not to estimate an
  optimal production concurrency. At workers>=2 each condition has one replicate
  per worker; the larger chunk reuse bound is verified by regression preparation
  counts, without attributing a separate timing gain to it in this worker grid.
  Examine elapsed/startup ratios and uncertainty
  before extrapolating. Run worker sections separately from other native jobs.
  Compile time is reported separately from bootstrap wall time. Both throughput
  including pool startup and throughput after startup are recorded. Whole-process
  CPU/RSS includes compilation, initialization and saving artifacts; the workload
  marker metrics isolate the bootstrap calls. Serial retained full results and
  worker summary results are explicitly labeled, never pooled into one rate.
  Generating theta and the supplied pristine initial state are created before
  timing; the bootstrap's own validation/reconstruction/cloning remain timed.
  Linux /proc sampling is every 100 ms across parent, workers and CmdStan
  descendants/session members. It can miss short-lived CPU and transient peaks;
  summed RSS can double-count shared pages. It is not unique/PSS memory. The
  driver records host load and counts actual physical cores from sysfs/affinity,
  respects CPU quotas and reserves 20% of physical cores. It refuses infeasible
  worker*parallel_chains settings and stops its own cell if combined sampled RSS
  exceeds 70% of live MemAvailable. It never terminates unrelated processes.
  N229 alpha retains approximately 139 MiB of prepared design per worker; this
  is a lower bound, not a complete process-memory forecast. Preparation is inside
  worker chunks; dense designs are never transferred from the parent. Baseline
  component measurements separate retained bytes, allocated bytes and RSS.
  The original nine baseline profiles have an independent exact comparison to
  fresh post-#328 runs in baseline-profile-exact-equivalence.csv; their source
  files also match the installation manifest. They predate loaded-body hash
  instrumentation and retain their own script/provenance hashes separately.
  Profiles use disjoint stack categories; inclusive function totals overlap and
  must not be summed. Outcome synthesis and serialization use repeated component
  measurements when too short to resolve in a whole-workload profile. Bootstrap
  disables session persistence, so the checkpoint category measures state/log
  handling and checkpoint decisions; there are no disk checkpoints to remove.
  Transport microprofiles send real-size pristine specs and compact results;
  ordered-reduction measurements reuse actual outputs in the frozen reducer.
  These isolate stage overhead and do not claim native workload throughput.
  Full raw RDS/profiler/Stan output remains in the isolated local evidence tree;
  compact measured tables, hashes, validation summaries and scripts are versioned.
  Serial grids overlapped other bounded qualification/check jobs; host load is
  recorded. The worker throughput grid runs sequentially without other native
  benchmark jobs. Timings are single bounded-cell point estimates, not repeated
  trials or confidence intervals. Audit and /proc sampling overhead is included
  in elapsed time; CPU/RSS columns describe the R workload process family.
  results.txt reports actual completed coverage, failures/skips, performance and
  remaining limits. No claim of optimal production concurrency is implied.
