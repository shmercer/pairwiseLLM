# Adaptive ranking

Initialize an adaptive ranking session and canonical state object.

## Usage

``` r
adaptive_rank_start(
  items,
  seed = 1L,
  session_dir = NULL,
  persist_item_log = FALSE,
  ...,
  adaptive_config = NULL
)
```

## Arguments

- items:

  A vector or data frame of items. Data frames must include an `item_id`
  column (or `id`/`ID`). Item IDs may be character; internal logs use
  integer indices derived from these IDs.

- seed:

  Integer seed used for deterministic warm-start shuffling and selection
  randomness.

- session_dir:

  Optional directory for saving session artifacts.

- persist_item_log:

  Logical; when TRUE, write per-refit item logs to disk.

- ...:

  Internal/testing only. Supply `now_fn` to override the clock used for
  timestamps.

- adaptive_config:

  Optional named list overriding adaptive controller behavior. Supported
  fields:

  `global_identified_reliability_min`, `global_identified_rank_corr_min`

  :   Thresholds used to mark global identifiability after each refit.

  `p_long_low`, `p_long_high`

  :   Posterior probability gate used for long-link eligibility once
      globally identified.

  `long_taper_mult`, `long_frac_floor`, `mid_bonus_frac`

  :   Late-stage long-link taper and quota reallocation controls.

  `explore_taper_mult`

  :   Late-stage exploration taper multiplier.

  `boundary_k`, `boundary_window`, `boundary_frac`

  :   Local-stage boundary-priority controls after global
      identifiability.

  `p_star_override_margin`, `star_override_budget_per_round`

  :   Near-tie star-cap override controls.

  Unknown fields and invalid values abort with an actionable error.

## Value

An adaptive state object containing `step_log`, `round_log`, and
`item_log`. The object includes class `"adaptive_state"`, item ID
mappings, TrueSkill state, warm-start queue, refit metadata, and runtime
configuration.

## Details

This function creates the stepwise controller state and seeds all
canonical logs used in the adaptive pairing workflow. Warm start pair
construction follows the shuffled chain design, which guarantees a
connected comparison graph after \\N - 1\\ committed comparisons.

Pair selection in this framework is TrueSkill-driven and uses base
utility \$\$U_0 = p\_{ij}(1 - p\_{ij})\$\$ where \\p\_{ij}\\ is the
current TrueSkill win probability for pair \\\\i, j\\\\. Bayesian BTL
posterior draws are not used for pair selection; they are used for
posterior inference, diagnostics, and stopping at refit rounds.

The returned state contains canonical logs:

- `step_log`: one row per attempted step,

- `round_log`: one row per posterior refit,

- `item_log`: per-item posterior summaries by refit.

If `session_dir` is supplied, the initialized state is persisted
immediately using
[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md).

## See also

[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 11)
summarize_adaptive(state)
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       3               0               0        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
```
