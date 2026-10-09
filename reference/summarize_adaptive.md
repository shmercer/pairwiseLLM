# Summarize an adaptive state.

Summarize an adaptive state.

## Usage

``` r
summarize_adaptive(
  state,
  include_starvation = FALSE,
  include_bootstrap = FALSE
)
```

## Arguments

- state:

  Adaptive state.

- include_starvation:

  Logical; append a `starvation_diagnostic` list-column containing
  terminal hybrid exhaustion evidence. Default FALSE.

- include_bootstrap:

  Logical; add a `bootstrap` list-column with policy, version,
  initialization seed, integrity identities, and saved tree diagnostics.
  Default FALSE retains the historical summary columns. Legacy sessions
  report the shuffled policy and no predictive bootstrap digest or
  diagnostics.

## Value

A one-row tibble with columns `n_items`, `steps_attempted`,
`committed_pairs`, `n_refits`, `last_stop_decision`, and
`last_stop_reason`, plus the optional `starvation_diagnostic` and
`bootstrap` list-columns.

## Details

Returns a compact run-level summary from canonical logs: attempted
steps, committed comparisons, refit count, and last stop
decision/reason. This is a pure view and does not recompute model
quantities.

The optional diagnostic is NULL when no current terminal evidence is
available, including sessions saved before this diagnostic was
introduced. Otherwise it contains a classification, originating step,
committed count, active item-set scope, maximum observations per pair,
and a remaining arithmetic capacity upper bound. The bound includes
bootstrap history and ignores pairing restrictions; it is not a
feasibility forecast. Sparse replay counts only unused allowed edges.
The attempt table retains each failed stage's fallback policies, filter
counts, pre-exposure hard-filter boundary, admissible candidate count,
and bounded-search flag. Counts refer to examined pools; overlapping
pools must not be summed. A missing bounded-search flag means its extent
was not recorded. Classification distinguishes pair-capacity exhaustion,
duplicate-policy exhaustion, exposure/star-cap exhaustion, other or
mixed restrictions, unknown evidence, and selection inconsistency
(surviving candidates despite starvation). Only zero arithmetic capacity
establishes global pair-capacity exhaustion. Other classifications
describe observed filter collapse, not proof of global infeasibility.
Existing stop reasons and canonical logs are unchanged.

## See also

[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`base::print()`](https://rdrr.io/r/base/print.html)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md),
[`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md),
[`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
summarize_adaptive(state)
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       3               0               0        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
```
