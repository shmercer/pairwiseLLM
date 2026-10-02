# Schedule-aware parametric bootstrap bias correction for Bradley-Terry scores

Simulate binary comparisons from fitted BT scores, refit the same
estimator, and estimate itemwise bias. Adaptive runs repeat the actual
package scheduling algorithm, including changes driven by simulated
earlier outcomes.

## Usage

``` r
bootstrap_bt_model(
  object,
  mode,
  n_rep,
  seed,
  schedule = NULL,
  initial_state = NULL,
  budget = NULL,
  estimator = NULL,
  estimator_args = list(),
  btl_config = NULL,
  schedule_fit_fn = NULL,
  min_success = n_rep,
  workers = 1L,
  keep = c("summary", "theta", "full")
)
```

## Arguments

- object:

  A fit from
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  or a named finite numeric vector of initial/generating BT scores. Item
  IDs must be unique. The initial estimate also supplies the generating
  probabilities; a separate generating model is not fitted. Positional,
  lapse, and tie models are outside this interface.

- mode:

  Required: `"fixed"` for an outcome-independent frozen schedule or
  `"adaptive"` to rerun within-set selection. Do not freeze an observed
  outcome-dependent adaptive schedule and describe it as schedule-aware.

- n_rep:

  Required integer number of bootstrap replicates, at least two. Choose
  this for the precision and cost of your application.

- seed:

  Required integer master seed, from zero to `.Machine$integer.max`.

- schedule:

  Fixed mode: a data frame with `object1`/`object2` or `A_id`/`B_id`.
  Row order, presentations and repeated pairs are retained. Other
  columns, including observed outcomes, are ignored. If omitted, recover
  `object$comparisons` when available.

- initial_state:

  Adaptive mode: a pristine
  [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md)
  state, before any comparison attempts or refits. Its connected N - 1
  initial pairs, initial presentations, predictive initialization,
  pairing strategy and constraints remain fixed. Predictive scores must
  be outcome-independent. Sparse reservoir membership and presentations
  are retained, but original reservoir outcomes are not used. Sessions
  are never written to disk.

- budget:

  Total number of committed comparisons, including initialization.
  Required in adaptive mode; in fixed mode defaults to the number of
  schedule rows and must equal that count.

- estimator:

  `"alpha"`, `"brglm2"`, or a function with arguments `bt_data` and
  `item_ids`, plus any `estimator_args`. A function returns a named
  finite theta vector or a fit with a `theta` data frame (`ID`,
  `theta`). It must raise an error for unsuccessful estimation. Reported
  failed convergence/uncertainty diagnostics invalidate a returned fit.
  For alpha/Firth fitted objects, recover the original
  estimator/settings and reject conflicting overrides. Other inputs
  require an explicit estimator. No automatic engine fallback.

- estimator_args:

  Named estimator arguments. Alpha requires an explicit `alpha`;
  numerical controls follow
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

- btl_config:

  Adaptive scheduling-refit configuration, passed to
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).
  Defaults to the saved configuration or canonical defaults. This is
  separate from the final `estimator`. Refits are never silently
  disabled: the default scheduling fitter requires optional CmdStan. An
  explicitly larger-than-budget `refit_pairs_target` describes a design
  without scheduled refits during this budget.

- schedule_fit_fn:

  Optional adaptive scheduling fitter with the existing
  `function(state, config)` posterior-fit contract. Omitted means the
  canonical Bayesian fitter. Bootstrap supplies deterministic
  `config$cmdstan$seed` at each refit. Custom functions must honor it
  for external RNGs and must be deterministic, provider-free, and
  independent of mutable external state.

- min_success:

  Minimum successful replicates, from two through `n_rep`. Default
  requires all replicates. Failures are never included in averages.

- workers:

  Positive worker count, default one (serial). More than one uses
  optional `future` and `future.apply` with a temporary multisession
  plan. The previous plan and caller RNG state are restored.

- keep:

  Retention: `"summary"` (default) keeps summaries and compact
  diagnostics; `"theta"` also keeps aligned replicate estimates;
  `"full"` additionally keeps comparisons, adaptive states and detailed
  failure diagnostics (which can include dense matrices). Full artifacts
  can be large.

## Value

A `pairwiseLLM_bt_bootstrap` list containing `theta` (ID, initial theta,
bootstrap mean, bias, corrected theta, bootstrap SD and Monte Carlo
error), `n_success`, `n_failed`, `status`, `replicates` (seeds, failures
and schedule diagnostics), `failures` (available error diagnostics),
optional `draws` and `artifacts`, and `provenance`. Provenance includes
normalized generating parameters, schedule/initial state,
estimator/settings, master seed, RNG convention, scheduling
configuration, callbacks and software versions.

## Details

Each outcome has probability `plogis(theta[A] - theta[B])`. Initial
scores and refits are aligned by ID and centered to sum zero. With
successful refits only, `bias = mean(theta_boot) - theta_initial` and
`theta_corrected = theta_initial - bias`. Corrections can change
rankings and need not improve every item or every application.

Adaptive p50, Pollitt-inspired, hybrid and random strategies use the
canonical live runner with a synthetic judge. Initialization stays
frozen, while later selector randomness changes by replicate. Scheduled
refits, TrueSkill updates, hybrid identifiability/tapering and
constraints remain active. The existing `max_pairs_after_stop`
continuation control is set to `budget` so statistical stopping cannot
truncate the requested fixed budget. Terminal exhaustion is a failed
replicate; it never triggers replacement with another strategy.

RNG algorithm is Mersenne-Twister, with Inversion normals and Rejection
sampling. For replicate b and stage s (selector, outcomes, scheduling
refits, final estimator, numbered 1–4), the integer seed is
`max(1, floor((seed * 1000003 + b * 10007 + s * 101 + 304) %% 2147483647))`.
Scheduling-refit seeds use the canonical stage derivation again, with
the scheduling seed, committed comparison count, stage 1, and offset
zero. Serial and parallel results use the same seeds and aggregation
order. Exact reproducibility requires compatible R/package/engine
versions and deterministic callbacks. There is no checkpoint/resume API;
restarting repeats the run.

`bootstrap_sd` describes replicate-estimate dispersion, whereas
`mcse_bias` is `bootstrap_sd / sqrt(n_success)`: simulation error in the
estimated bias. Neither is automatically a standard error for the
bias-corrected estimator. This function does not construct confidence
intervals or corrected SSR.

An unmet `min_success` raises `pairwiseLLM_bt_bootstrap_error` with a
`result` field containing diagnostics and missing bias/corrected scores.
Tolerated failures raise a warning and remain in the diagnostic table.
Numerical warnings from successful fits are recorded per replicate and
signaled once at completion.

## See also

[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)

Other frequentist models:
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`predict.pairwiseLLM_bt_alpha()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_alpha.md),
[`predict.pairwiseLLM_bt_firth()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_firth.md),
[`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

## Examples

``` r
dat <- data.frame(object1 = rep("a", 8), object2 = rep("b", 8),
                  result = c(rep(1, 6), rep(0, 2)))
fit <- fit_bt_model(dat, engine = "alpha", alpha = 1, verbose = FALSE)
boot <- bootstrap_bt_model(fit, mode = "fixed", n_rep = 20, seed = 304)
boot$theta
#> # A tibble: 2 × 7
#>   ID    theta_initial bootstrap_mean    bias theta_corrected bootstrap_sd
#>   <chr>         <dbl>          <dbl>   <dbl>           <dbl>        <dbl>
#> 1 a             0.424          0.404 -0.0201           0.444        0.278
#> 2 b            -0.424         -0.404  0.0201          -0.444        0.278
#> # ℹ 1 more variable: mcse_bias <dbl>
```
