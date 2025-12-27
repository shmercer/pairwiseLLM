# Run a core-linking batch workflow end-to-end (round-based)

This runner orchestrates a multi-wave (batch) workflow using a stable
core linking set. For each batch of new items, it runs a round-based
loop: propose pairs (core↔new + new↔new + optional core↔core audit),
score the pairs via `judge_fun`, append results, fit a BT model, compute
stop metrics on the batch's new IDs (optionally including core drift),
then stop or continue.

## Usage

``` r
bt_run_core_linking(
  samples,
  batches,
  core_ids = NULL,
  core_method = c("embeddings", "token_stratified", "random"),
  core_size = 30,
  embeddings = NULL,
  judge_fun,
  initial_results = NULL,
  judge = NULL,
  fit_fun = fit_bt_model,
  build_bt_fun = build_bt_data,
  engine = "sirt",
  fit_verbose = FALSE,
  return_diagnostics = TRUE,
  include_residuals = FALSE,
  round_size = 50,
  max_rounds_per_batch = 50,
  within_batch_frac = 0.25,
  core_audit_frac = 0.1,
  k_neighbors = 10,
  min_judgments = 12,
  forbid_repeats = TRUE,
  balance_positions = TRUE,
  se_probs = c(0.5, 0.9, 0.95),
  fit_bounds = c(0.7, 1.3),
  reliability_target = 0.9,
  sepG_target = 3,
  rel_se_p90_target = 0.3,
  rel_se_p90_min_improve = 0.01,
  max_item_misfit_prop = 0.05,
  max_judge_misfit_prop = 0.05,
  core_theta_cor_target = NA_real_,
  core_theta_spearman_target = NA_real_,
  core_max_abs_shift_target = NA_real_,
  core_p90_abs_shift_target = NA_real_,
  drift_reference = c("previous_round", "baseline"),
  seed = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- samples:

  A tibble/data.frame with columns `ID` and `text`. `ID` must be unique
  and non-missing.

- batches:

  A non-empty list where each element is a character vector of IDs to be
  added in that batch. IDs must be present in `samples$ID`.

- core_ids:

  Optional character vector of core IDs. If `NULL`, core IDs are
  selected using
  [`select_core_set`](https://shmercer.github.io/pairwiseLLM/reference/select_core_set.md).

- core_method:

  Core selection method used when `core_ids` is `NULL`. Passed to
  [`select_core_set`](https://shmercer.github.io/pairwiseLLM/reference/select_core_set.md).

- core_size:

  Core size used when `core_ids` is `NULL`.

- embeddings:

  Optional embedding matrix for `core_method = "embeddings"`.

- judge_fun:

  Function that accepts a tibble of pairs with columns `ID1`, `text1`,
  `ID2`, `text2` and returns a tibble with columns `ID1`, `ID2`,
  `better_id`. If `judge` is provided, the output must also include that
  column.

- initial_results:

  Optional tibble of previously-judged results (same schema as output of
  `judge_fun`). Used as a warm start.

- judge:

  Optional string naming the judge column to pass through to modeling.

- fit_fun:

  Function that fits a BT model from BT data (default
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)).

- build_bt_fun:

  Function to build BT data from results (default
  [`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)).

- engine:

  Passed to `fit_fun` when `fit_fun = fit_bt_model`.

- fit_verbose:

  Passed to `fit_fun` when `fit_fun = fit_bt_model`.

- return_diagnostics:

  Passed to `fit_fun` when `fit_fun = fit_bt_model`.

- include_residuals:

  Passed to `fit_fun` when `fit_fun = fit_bt_model`.

- round_size:

  Target number of pairs proposed per round (per batch).

- max_rounds_per_batch:

  Maximum rounds to run for each batch.

- within_batch_frac:

  Fraction of each round allocated to new↔new comparisons.

- core_audit_frac:

  Fraction of each round allocated to core↔core audit comparisons.

- k_neighbors:

  Passed to
  [`select_core_link_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_core_link_pairs.md).

- min_judgments:

  Passed to
  [`select_core_link_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_core_link_pairs.md).

- forbid_repeats:

  Forbid repeat unordered pairs across the entire run.

- balance_positions:

  Balance positions (ID1 vs ID2) when proposing pairs.

- se_probs:

  Passed to
  [`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md).

- fit_bounds:

  Passed to
  [`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md)
  when diagnostics are available.

- reliability_target:

  Passed to
  [`bt_should_stop`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md).

- sepG_target:

  Passed to
  [`bt_should_stop`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md).

- rel_se_p90_target:

  Passed to
  [`bt_should_stop`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md).

- rel_se_p90_min_improve:

  Passed to
  [`bt_should_stop`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md).

- max_item_misfit_prop:

  Passed to
  [`bt_should_stop`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md).

- max_judge_misfit_prop:

  Passed to
  [`bt_should_stop`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md).

- core_theta_cor_target:

  Optional drift guardrail for Pearson correlation (default `NA` =
  disabled).

- core_theta_spearman_target:

  Optional drift guardrail for Spearman correlation (default `NA` =
  disabled).

- core_max_abs_shift_target:

  Optional drift guardrail for maximum abs shift (default `NA` =
  disabled).

- core_p90_abs_shift_target:

  Optional drift guardrail for p90 abs shift (default `NA` = disabled).

- drift_reference:

  Drift reference for computing core drift metrics: `"previous_round"`
  compares to the prior round's fit; `"baseline"` compares to a fixed
  baseline fit.

- seed:

  Optional integer seed used to make pair proposal reproducible across
  runs.

- verbose:

  Logical; print minimal progress per batch/round.

- ...:

  Additional arguments forwarded to `fit_fun`.

## Value

A list with:

- core_ids:

  Core linking IDs used.

- batches:

  Normalized batches list.

- results:

  All judged results (canonicalized `better_id`).

- fits:

  List of per-round fits (including bootstrap/warm start).

- final_fits:

  Named list of final fit per batch (plus `"bootstrap"`).

- metrics:

  Tibble of stop metrics per round (computed on batch new IDs).

- batch_summary:

  One row per batch: rounds used, stop reason, counts.

## Details

Stopping is typically driven by precision on the batch's new items
(e.g., `rel_se_p90`) and can be gated by core drift guardrails (via
`core_*_target` thresholds).

## Examples

``` r
# CRAN-safe example (no APIs, no sirt): deterministic simulated judging + mock fit.
samples <- tibble::tibble(
  ID = LETTERS[1:6],
  text = paste("text", LETTERS[1:6])
)
batches <- list(batch1 = c("D", "E"), batch2 = c("F"))
core_ids <- c("A", "B", "C")

# Deterministic simulated judge (always picks the higher true theta)
true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2, F = -3)
judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

# Tiny mock fit: returns required structure (ID/theta/se)
round <- 0
mock_fit <- function(bt_data, ...) {
  round <<- round + 1
  ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
  se <- rep(max(0.60 - 0.15 * round, 0.05), length(ids))
  list(
    engine = "mock",
    reliability = NA_real_,
    theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = se),
    diagnostics = list(sepG = NA_real_)
  )
}

out <- bt_run_core_linking(
  samples = samples,
  batches = batches,
  core_ids = core_ids,
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  round_size = 8,
  max_rounds_per_batch = 3,
  # disable thresholds requiring sirt diagnostics for this example
  reliability_target = NA_real_,
  sepG_target = NA_real_,
  max_item_misfit_prop = NA_real_,
  max_judge_misfit_prop = NA_real_,
  rel_se_p90_target = 0.80,
  verbose = FALSE
)
out$batch_summary
#> # A tibble: 2 × 5
#>   batch_index n_requested n_new rounds_used stop_reason
#>         <int>       <int> <int>       <int> <chr>      
#> 1           1           2     2           1 stopped    
#> 2           2           1     1           2 no_pairs   
```
