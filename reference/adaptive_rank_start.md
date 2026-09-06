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
  adaptive_config = NULL,
  checkpoint_every_steps = NULL
)
```

## Arguments

- items:

  A vector or data frame of items. Data frames must include an `item_id`
  column (or `id`/`ID`). For linking run modes, items must also include
  integer `set_id` values and globally unique `global_item_id` values.
  Item IDs may be character; internal logs use integer indices derived
  from these IDs.

- seed:

  Integer seed used for deterministic warm-start shuffling and selection
  randomness. Default is `1L`.

- session_dir:

  Optional directory for saving session artifacts. Default is `NULL`.

- persist_item_log:

  Logical; when TRUE, write per-refit item logs to disk. Default is
  `FALSE`.

- ...:

  Internal/testing only. Supply `now_fn` to override the clock used for
  timestamps.

- adaptive_config:

  Optional named list of adaptive controller overrides. Unknown fields
  and invalid values abort with an actionable error. See
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
  for the full list of supported keys, detailed semantics, and defaults.

- checkpoint_every_steps:

  Optional positive integer checkpoint cadence for ordinary live
  persistence. If `NULL`, defaults to `100L`.

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

Pair selection in this framework is stepwise and uncertainty-aware.
Within-set routing uses TrueSkill base utility \$\$U_0 = p\_{ij}(1 -
p\_{ij})\$\$ where \\p\_{ij}\\ is the current TrueSkill win probability
for pair \\\\i, j\\\\. In linking Phase B, anchor/strata routing uses a
linking-global score derived from Phase A raw summaries and the accepted
Phase B linking state. In linking Phase B, eligible cross-set candidates
are ranked by ridge-stabilized D-optimal log-det information gain on the
active linking parameter block using order-averaged Model D
probabilities. In the spoke free block with the hub fixed. Linking
inference parameters are used for inference/diagnostics/stopping, not as
direct selection objectives. Phase B uses pooled within-set Phase A
judge-parameter estimates, using the configured BTL model variant, as
the accepted shared source for fixed `beta`/`epsilon` constants.
Bayesian BTL posterior draws are not used as general pair-selection
objectives; within-set pairing remains TrueSkill-routed, with accepted
posterior refits contributing only to the long-link probability gate.
Linking Phase B refits use Bayesian posterior estimation and posterior
summaries/diagnostics are logged per spoke at each linking refit.

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
