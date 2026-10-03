# Bootstrap bias correction for BT scores

## What the correction estimates

The [integrated CJ
workflow](https://shmercer.github.io/pairwiseLLM/articles/adaptive-cj-workflow.md)
explains where bootstrap bias correction fits alongside pair selection,
internal reliability, score recovery, and held-out prediction. This
guide focuses on the bootstrap interface. The examples require the
suggested `withr` package and are displayed without execution when it is
absent.

A Bradley–Terry (BT) score describes relative strength on a log-odds
scale. With limited comparisons, an estimator may systematically place
scores too far apart or too close together. A **parametric bootstrap**
estimates this bias by simulating new judgments from the fitted scores
and fitting the same estimator to each simulated dataset. It makes no
provider calls.

Pair selection matters. For a random schedule chosen independently of
results, the bootstrap can keep the pairs fixed. For an adaptive
schedule, later pairs depend on earlier winners. Each replicate must
repeat that selection process using its own simulated winners. Reusing
only the observed adaptive pairs would answer a different question.

The function aligns item IDs and centers every score vector to sum to
zero. For each item it calculates:

    estimated bias = average bootstrap estimate - initial estimate
    corrected score = initial estimate - estimated bias

Zero remains a relative reference, not a passing score. The correction
can change rankings, and it does not guarantee improvement for every
item or application.

## A fixed random schedule

This synthetic example uses a connected starting chain and additional
random pairs, all chosen before simulating any winners. Alpha adjustment
is used here so the example needs no optional fitting engine; the
penalty is specified in advance. Firth (`engine = "brglm2"`) is also
supported for nonadaptive schedules when that optional package is
installed.

``` r

ids <- letters[1:4]
truth <- setNames(c(-0.8, -0.2, 0.2, 0.8), ids)
dat <- withr::with_seed(304, {
  extra <- replicate(21, sample(ids, 2))
  pairs <- data.frame(object1 = c(ids[1:3], extra[1, ]),
                      object2 = c(ids[2:4], extra[2, ]))
  p <- plogis(truth[pairs$object1] - truth[pairs$object2])
  pairs$result <- as.integer(runif(nrow(pairs)) < p)
  pairs
})
fit <- fit_bt_model(dat, engine = "alpha", alpha = 0.5, verbose = FALSE)
fixed <- bootstrap_bt_model(fit, mode = "fixed", n_rep = 20, seed = 1304)
fixed$theta
#> # A tibble: 4 × 7
#>   ID    theta_initial bootstrap_mean    bias theta_corrected bootstrap_sd
#>   <chr>         <dbl>          <dbl>   <dbl>           <dbl>        <dbl>
#> 1 a            -0.320         -0.138  0.182           -0.502        0.486
#> 2 b            -0.384         -0.327  0.0569          -0.441        0.367
#> 3 c             0.404          0.240 -0.165            0.569        0.483
#> 4 d             0.299          0.225 -0.0742           0.373        0.565
#> # ℹ 1 more variable: mcse_bias <dbl>
fixed$provenance[c("mode", "seed", "pairing_strategy", "refit", "versions")]
#> $mode
#> [1] "fixed"
#> 
#> $seed
#> [1] 1304
#> 
#> $pairing_strategy
#> [1] "fixed"
#> 
#> $refit
#> $refit$estimator
#> [1] "alpha"
#> 
#> $refit$args
#> $refit$args$alpha
#> [1] 0.5
#> 
#> 
#> 
#> $versions
#>           R pairwiseLLM       stats 
#>     "4.6.1"     "1.6.0"     "4.6.1"
```

The fitted object supplies both the generating scores and the estimator
settings. Each replicate keeps the original ordered pair rows, including
any repetitions, and draws fresh winners. Twenty replicates keep this
example quick; that number is not a recommendation for a study.

## An adaptive p50 schedule

Save the initial state **before collecting any outcomes**. Here the p50
strategy chooses partners with current TrueSkill win probabilities near
one half. The synthetic judge below supplies winners in place of a
provider.

``` r

ids <- letters[1:6]
truth <- setNames(seq(-1, 1, length.out = 6), ids)
initial <- adaptive_rank_start(ids, seed = 17,
  adaptive_config = list(pairing_strategy = "trueskill_p50"))
judge <- function(A, B, state, ...) {
  p <- plogis(truth[[A$item_id]] - truth[[B$item_id]])
  list(is_valid = TRUE, Y = as.integer(runif(1) < p))
}
# This demonstration design has no Bayesian refit within its 12-comparison budget.
schedule_config <- list(refit_pairs_target = 100L)
observed <- withr::with_seed(7304, adaptive_rank_run_live(initial, judge,
  n_steps = 12, btl_config = schedule_config, progress = "none"))
dat <- adaptive_results_history(observed)
fit <- fit_bt_model(dat, engine = "alpha", alpha = 0.5, verbose = FALSE)
adaptive <- bootstrap_bt_model(fit, mode = "adaptive", n_rep = 4, seed = 2304,
  initial_state = initial, budget = 12, btl_config = schedule_config)
adaptive$theta
#> # A tibble: 6 × 7
#>   ID    theta_initial bootstrap_mean    bias theta_corrected bootstrap_sd
#>   <chr>         <dbl>          <dbl>   <dbl>           <dbl>        <dbl>
#> 1 a            -2.61          -2.14   0.469           -3.08         0.601
#> 2 b            -0.870         -0.197  0.674           -1.54         0.571
#> 3 c            -0.715         -0.648  0.0667          -0.782        0.681
#> 4 d             0.397         -0.453 -0.851            1.25         1.17 
#> 5 e             2.44           1.71  -0.724            3.16         0.489
#> 6 f             1.36           1.73   0.366            0.993        0.395
#> # ℹ 1 more variable: mcse_bias <dbl>
adaptive$replicates[, c("replicate", "success", "n_comparisons", "schedule_digest")]
#> # A tibble: 4 × 4
#>   replicate success n_comparisons schedule_digest                 
#>       <int> <lgl>           <int> <chr>                           
#> 1         1 TRUE               12 8e73059d9ff495e86aa26ef53c6ff117
#> 2         2 TRUE               12 3e80c6940736310370db99ad7add564a
#> 3         3 TRUE               12 a45fe7069b80fe0680ab99a7654b2558
#> 4         4 TRUE               12 005c33660e3e93f0b85b2ddb91fb6208
adaptive$provenance[c("mode", "seed", "pairing_strategy", "refit", "versions")]
#> $mode
#> [1] "adaptive"
#> 
#> $seed
#> [1] 2304
#> 
#> $pairing_strategy
#> [1] "trueskill_p50"
#> 
#> $refit
#> $refit$estimator
#> [1] "alpha"
#> 
#> $refit$args
#> $refit$args$alpha
#> [1] 0.5
#> 
#> 
#> 
#> $versions
#>           R pairwiseLLM       stats 
#>     "4.6.1"     "1.6.0"     "4.6.1"
```

The initial connected pairs and their presentations stay fixed across
replicates. After initialization, each replicate has a new deterministic
selector seed and its own simulated outcomes. The selector and state
updates are the same ones used by the ordinary adaptive runner. Four
replicates here illustrate the workflow only; their Monte Carlo error
can be substantial.

The same interface supports `trueskill_pollitt`, `hybrid`, and `random`
states. Keep the original scheduling-refit settings when reproducing a
design. Hybrid routing can change after Bayesian refits establish a
sufficiently identified global scale. Those refits are retained and may
require CmdStan. The final bootstrap `estimator` is a separate choice
from the scheduling fitter. An outcome-independent predictive prior may
be included in the initial state. Sparse reservoirs restrict selection
to their legal pairs; their old winners are discarded and new binary
outcomes are simulated.

The budget counts successful comparisons, including the initial
connected structure. Statistical stopping is recorded but does not
truncate this fixed-budget experiment. If legal selection is exhausted,
the replicate fails. Small panels can exhaust hybrid exposure
constraints before using every possible pair; the bootstrap does not
relax these constraints to fill the budget.

## Reading the results and managing cost

`bias` estimates systematic error under the fitted generating model.
`bootstrap_sd` describes how much the replicate estimates vary.
`mcse_bias` describes simulation error in the estimated bias and usually
decreases as the replicate count increases. Neither quantity is
automatically a standard error for the corrected score. The function
does not provide confidence intervals or calculate corrected
scale-separation reliability (SSR).

Inspect `n_success`, `n_failed`, and `replicates` before using a
correction. By default every requested replicate must succeed. You may
choose a lower `min_success` explicitly, but excluded failures can
change the interpretation: the average then describes successful refits.
An unmet threshold raises an error whose `result` field retains
diagnostics, with missing corrected scores. No failed estimates are
silently included and no other estimator is substituted.

Each replicate costs a schedule simulation and final refit; hybrid can
also require multiple Bayesian fits. Use `workers = 2` for parallel
execution with the optional `future` and `future.apply` packages.
Reproducible seeds and ordered aggregation give the same scientific
results as serial execution. Avoid oversubscribing CPUs with both
bootstrap workers and many sampler chains. Custom callbacks must honor
supplied seeds and avoid mutable external state.

The default `keep = "summary"` discards large replicate states. Use
`"theta"` to retain score draws, or `"full"` when individual schedules
or detailed numerical failure diagnostics need inspection. Detailed
failures can contain large matrices. Save the returned object with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) to retain inputs,
settings, seeds, software versions and diagnostics. There is no
checkpoint/resume interface; restarting with the same inputs and
compatible software repeats the run.

## Citation

> Mercer, S. H. (2026). *Bootstrap bias correction for BT scores* \[R
> package vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
