# Run one adaptive round: compute metrics, decide stopping, and propose next pairs

This helper is intended for round-based adaptive comparative judgement
workflows. Given a current Bradley–Terry fit (typically from
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)),
it computes stopping metrics, applies stopping rules, and (if not
stopping) proposes a new set of pairs for the next round using
[`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

## Usage

``` r
bt_adaptive_round(
  samples,
  fit,
  existing_pairs = NULL,
  prev_metrics = NULL,
  round_size,
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
  seed = NULL
)
```

## Arguments

- samples:

  A tibble/data frame with columns `ID` and `text`.

- fit:

  A list returned by
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  containing at least `$theta` with columns `ID`, `theta`, and `se`.

- existing_pairs:

  Optional data frame of previously judged pairs (used for repeat
  prevention, judgment counts, and position balancing). Supported
  formats:

  - `ID1`, `ID2`

  - `object1`, `object2`

- prev_metrics:

  Optional one-row tibble of prior-round metrics (as returned by
  [`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md)),
  used for stability-based stopping.

- round_size:

  Integer number of new pairs to propose for the next round.

- se_probs:

  Numeric vector of probabilities for SE quantiles passed to
  [`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md).
  Default: `c(0.5, 0.9, 0.95)`.

- fit_bounds:

  Numeric length-2 vector giving lower/upper acceptable infit/outfit
  bounds. Default: `c(0.7, 1.3)`.

- reliability_target:

  Optional numeric. If not `NA`, require
  `metrics$reliability >= reliability_target`.

- sepG_target:

  Optional numeric. If not `NA`, require `metrics$sepG >= sepG_target`.

- rel_se_p90_target:

  Optional numeric. If not `NA`, require
  `metrics$rel_se_p90 <= rel_se_p90_target` to meet the precision
  target.

- rel_se_p90_min_improve:

  Optional numeric. If not `NA` and `prev_metrics` is provided, allow
  stopping when improvement in `rel_se_p90` stalls.

- max_item_misfit_prop:

  Optional numeric. If not `NA`, require
  `metrics$item_misfit_prop <= max_item_misfit_prop` (when available).

- max_judge_misfit_prop:

  Optional numeric. If not `NA`, require
  `metrics$judge_misfit_prop <= max_judge_misfit_prop` (when available).

- k_neighbors:

  Integer number of adjacent neighbors (in theta order) to consider in
  candidate generation. Passed to
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

- min_judgments:

  Integer minimum desired number of judgments per item. Passed to
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

- forbid_repeats:

  Logical; passed to
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

- balance_positions:

  Logical; passed to
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

- seed:

  Optional integer seed for reproducibility; passed to
  [`select_adaptive_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md).

## Value

A list with:

- metrics:

  One-row tibble from
  [`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md).

- decision:

  List from
  [`bt_should_stop`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md)
  containing `stop`, `details`, and `improve`.

- pairs_next:

  Tibble of proposed pairs with columns `ID1`, `text1`, `ID2`, `text2`.

## Details

This function does not fit the model itself; it expects you to provide a
fit. A typical loop is:

1.  Fit a BT model using current results

2.  Call `bt_adaptive_round()` to get `pairs_next`

3.  Score `pairs_next` (LLM/humans), append results, repeat

## Examples

``` r
samples <- tibble::tibble(
  ID = c("A", "B", "C"),
  text = c("text A", "text B", "text C")
)

# Mock fit object (for example/documentation)
fit <- list(
  engine = "mock",
  reliability = 0.95,
  theta = tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0.0, 0.1, 0.2),
    se = c(0.5, 0.5, 0.5)
  ),
  diagnostics = list(sepG = 3.5)
)

out <- bt_adaptive_round(samples, fit, round_size = 2, seed = 1)
out$decision$stop
#> [1] FALSE
out$pairs_next
#> # A tibble: 2 × 4
#>   ID1   text1  ID2   text2 
#>   <chr> <chr>  <chr> <chr> 
#> 1 A     text A B     text B
#> 2 B     text B C     text C
```
