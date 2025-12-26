# Run a round-based adaptive BT workflow end-to-end

This exported runner implements the core adaptive loop:

1.  propose pairs (initial random bootstrap, then adaptive),

2.  score pairs using a provided judge function (LLM/human/simulator),

3.  append results,

4.  fit a BT model,

5.  compute stopping metrics and decide whether to stop,

6.  repeat until stopping criteria are met or `max_rounds` is reached.

## Usage

``` r
bt_run_adaptive(
  samples,
  judge_fun,
  initial_results = NULL,
  judge = NULL,
  engine = "sirt",
  fit_verbose = FALSE,
  return_diagnostics = TRUE,
  include_residuals = FALSE,
  round_size = 50,
  init_round_size = round_size,
  max_rounds = 50,
  se_probs = c(0.5, 0.9, 0.95),
  fit_bounds = c(0.7, 1.3),
  reliability_target = 0.9,
  sepG_target = 3,
  rel_se_p90_target = 0.3,
  rel_se_p90_min_improve = 0.01,
  max_item_misfit_prop = 0.05,
  max_judge_misfit_prop = 0.05,
  k_neighbors = 10,
  min_judgments = 12,
  forbid_repeats = TRUE,
  balance_positions = TRUE,
  seed_pairs = NULL,
  reverse_audit = FALSE,
  reverse_pct = 0.1,
  n_reverse = NULL,
  reverse_seed = NULL,
  fit_fun = fit_bt_model,
  build_bt_fun = build_bt_data,
  ...
)
```

## Arguments

- samples:

  A tibble/data.frame with columns `ID` and `text`. `ID` must be unique
  and non-missing; `text` is the content shown to the judge.

- judge_fun:

  A function that accepts a tibble of pairs with columns `ID1`, `text1`,
  `ID2`, `text2` and returns a tibble with columns `ID1`, `ID2`, and
  `better_id`. If `judge` is provided, the returned tibble must also
  include that judge column. `better_id` may be returned as the literal
  winning ID (`ID1` or `ID2`) or as common positional labels (e.g.,
  `SAMPLE_1`/`SAMPLE_2`, `ID1`/`ID2`, `1`/`2`, `0`/`1`, `A`/`B`,
  `LEFT`/`RIGHT`); these are normalized to the corresponding IDs.
  Blank/NA-like winners are treated as missing (`NA`) and are ignored in
  scoring.

- initial_results:

  Optional tibble/data.frame of already-scored pairs with columns `ID1`,
  `ID2`, `better_id` (and optional judge column). When provided, these
  results are used as the starting state (and no bootstrap is performed
  unless `initial_results` is empty).

- judge:

  Optional character scalar giving the name of the column in results
  that identifies the judge/backend/model (e.g., `"gpt4o"` vs
  `"claude"`). When provided, the column must be present in outputs from
  `judge_fun` and is passed to
  [`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
  so engines that support judge effects can use it.

- engine:

  Character scalar passed to `fit_fun` as its `engine` argument. Default
  `"sirt"`.

- fit_verbose:

  Logical; passed to `fit_fun` as `verbose`. Default `FALSE`.

- return_diagnostics:

  Logical; passed to `fit_fun`. If `TRUE`, attempt to return
  engine-specific diagnostics (e.g., item fit, separation/reliability).
  Default `TRUE`.

- include_residuals:

  Logical; passed to `fit_fun`. If `TRUE`, request residual/probability
  outputs when supported (may increase compute/memory). Default `FALSE`.

- round_size:

  Integer. Number of new pairs to propose and score in each adaptive
  round. If `0`, the runner will fit once (if possible) and then stop
  without proposing new pairs.

- init_round_size:

  Integer. Number of bootstrap (random) pairs to score before the first
  model fit when `initial_results` is `NULL` or empty. Default:
  `round_size`.

- max_rounds:

  Integer. Maximum number of adaptive rounds to run (excluding the
  bootstrap scoring step). Default `50`.

- se_probs:

  Numeric vector of probabilities in (0, 1) used when summarizing the
  distribution of standard errors for stopping diagnostics (e.g.,
  median, 90th percentile). Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

- fit_bounds:

  Numeric length-2 vector giving acceptable infit/outfit (or analogous)
  bounds when available. Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

- reliability_target:

  Numeric. Target reliability/separation-based criterion used by
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md)
  for stopping decisions.

- sepG_target:

  Numeric. Target separation index (or analogous) used by
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md)
  for stopping decisions.

- rel_se_p90_target:

  Numeric. Target value for the 90th percentile of item SE (or a
  comparable uncertainty summary) used for stopping. Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

- rel_se_p90_min_improve:

  Numeric. Minimum required improvement in the uncertainty summary
  relative to the previous round; if improvement falls below this,
  stopping may be allowed (depending on other criteria). Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

- max_item_misfit_prop:

  Numeric between 0 and 1 (inclusive). Maximum allowed proportion of
  item misfit flags (based on `fit_bounds`/diagnostics) before stopping
  is disallowed. Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

- max_judge_misfit_prop:

  Numeric between 0 and 1 (inclusive). Maximum allowed proportion of
  judge misfit flags before stopping is disallowed (when judge
  diagnostics are available). Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

- k_neighbors:

  Integer. When ability estimates are available, restrict candidate pair
  selection to approximately local neighborhoods in `theta` (e.g., near
  neighbors) to focus comparisons where they are most informative. If
  `theta` is not available (early rounds), selection falls back to
  non-theta heuristics. Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md)
  /
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

- min_judgments:

  Integer. Minimum number of total judgments per item to prioritize
  before focusing on adaptive informativeness/uncertainty. Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

- forbid_repeats:

  Logical. If `TRUE`, unordered pairs (A,B) are not repeated across
  rounds. Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md)
  /
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

- balance_positions:

  Logical. If `TRUE`, attempt to balance how often each item appears in
  the first vs second position (`ID1` vs `ID2`) to mitigate positional
  bias. Passed to
  [`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md)
  /
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

- seed_pairs:

  Optional integer seed used for bootstrap pair generation as
  `seed_pairs`, and for adaptive rounds as `seed_pairs + round`. The RNG
  state is restored to its prior value (or returned to "uninitialized"
  if it was missing). Note: this controls pair selection
  reproducibility; it does not control randomness inside `judge_fun`
  unless your `judge_fun` uses it explicitly.

- reverse_audit:

  Logical. If `TRUE`, run a post-stop reverse-order audit by selecting a
  subset of forward-scored pairs, reversing their order, and re-scoring
  them. This does not affect adaptive sampling decisions (it is
  post-hoc).

- reverse_pct:

  Numeric between 0 and 1 (inclusive). Proportion of eligible unique
  forward pairs to reverse for the audit. Eligible pairs are unique
  unordered forward pairs with non-missing `better_id`. Ignored if
  `n_reverse` is provided.

- n_reverse:

  Optional integer. Number of eligible unique forward pairs to reverse
  for the audit. If provided, overrides `reverse_pct`.

- reverse_seed:

  Optional integer seed used only for selecting which pairs to reverse
  (RNG state is restored afterward).

- fit_fun:

  Function used to fit the BT model. Default
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).
  Primarily intended as a test hook; most users should keep the default.

- build_bt_fun:

  Function used to convert results into BT data. Default
  [`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md).
  Primarily intended as a test hook.

- ...:

  Additional arguments passed through to `fit_fun`.

## Value

A list with elements:

- results:

  All accumulated forward-direction results (ID1, ID2, better_id, ...).

- bt_data:

  BT data built from `results`.

- fits:

  List of per-round fit objects (one per adaptive round).

- rounds:

  A tibble summarizing each adaptive round (metrics + stop flag).

- pairs_bootstrap:

  Pairs used in the bootstrap scoring step (may be empty).

- reverse_audit:

  NULL unless `reverse_audit=TRUE`; then contains audit pairs, reverse
  results, and consistency outputs.

## Details

**No reverse-order checks during adaptive sampling.** Optionally, after
stopping (or hitting `max_rounds`), you can run a post-hoc reverse-order
audit on a random subset of already-judged pairs and compute
forward-vs-reverse consistency via
[`compute_reverse_consistency`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md).

The default modeling function is
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
and the default stopping+pairing helper is
[`bt_adaptive_round`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md).

## Examples

``` r
# Minimal self-contained example that does not require sirt:
samples <- tibble::tibble(
  ID = c("A", "B", "C", "D"),
  text = paste("text", c("A", "B", "C", "D"))
)

# A tiny "judge" simulator (deterministic by latent ability):
true_theta <- c(A = 2, B = 1, C = 0, D = -1)
judge_fun <- function(pairs) {
  simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
}

# A tiny fit function (test-style), so the example runs without external engines:
fit_fun <- function(bt_data, ...) {
  bt_data <- as.data.frame(bt_data)
  ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
  wins <- stats::setNames(rep(0L, length(ids)), ids)
  n_j <- stats::setNames(rep(0L, length(ids)), ids)
  for (i in seq_len(nrow(bt_data))) {
    a <- as.character(bt_data[[1]][i])
    b <- as.character(bt_data[[2]][i])
    r <- as.numeric(bt_data[[3]][i])
    if (is.finite(r)) {
      if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
      n_j[a] <- n_j[a] + 1L
      n_j[b] <- n_j[b] + 1L
    }
  }
  theta <- as.numeric(wins - stats::median(wins))
  se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
  list(
    engine = "mock",
    reliability = 0.95,
    theta = tibble::tibble(ID = names(wins), theta = theta, se = se),
    diagnostics = list(sepG = 3.5)
  )
}

out <- bt_run_adaptive(
  samples = samples,
  judge_fun = judge_fun,
  fit_fun = fit_fun,
  engine = "mock",
  round_size = 2,
  init_round_size = 2,
  max_rounds = 2,
  rel_se_p90_target = NA_real_,
  rel_se_p90_min_improve = NA_real_
)
out$rounds
#> # A tibble: 1 × 18
#>   round n_new_pairs_scored n_total_results stop  engine n_items theta_sd se_mean
#>   <int>              <int>           <int> <lgl> <chr>    <int>    <dbl>   <dbl>
#> 1     1                  0               2 TRUE  mock         3    0.577   0.902
#> # ℹ 10 more variables: se_max <dbl>, rel_se_mean <dbl>, rel_se_p90 <dbl>,
#> #   reliability <dbl>, sepG <dbl>, item_misfit_prop <dbl>,
#> #   judge_misfit_prop <dbl>, se_p50 <dbl>, se_p90 <dbl>, se_p95 <dbl>
```
