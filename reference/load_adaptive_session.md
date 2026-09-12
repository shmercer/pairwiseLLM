# Load an adaptive session from disk.

Load an adaptive session from disk.

## Usage

``` r
load_adaptive_session(session_dir)
```

## Arguments

- session_dir:

  Directory containing session artifacts.

## Value

An `adaptive_state` object ready for resume.

## Details

Restores a persisted Adaptive state and revalidates basic invariants
such as schema version, required state fields, and index ranges in
`step_log`. If per-refit item logs are found on disk, they are loaded
into `state$item_log` and persistence is marked as enabled. Resume uses
strict schema validation for canonical logs; incompatible saved schemas
abort with explicit errors.

Legacy sessions without a saved predictive mode migrate to `cold` when
no predictive prior exists, and `btl_only` otherwise. An absent pairing
strategy migrates to `hybrid`. Saved TrueSkill values, the connected
shuffled bootstrap queue and its index, and round progress remain
authoritative; loading never recomputes predictions or initializes
TrueSkill again.

`metadata.rds` records effective `warm_start_mode` and
`pairing_strategy` for session-level audit. Direct step logs already
record `pairing_strategy`, the presented A-over-B TrueSkill probability
`p_ij`, and `target_distance` (missing for random pairing). Predictive
vectors and provenance are retained once in `state$predictive_prior`.

## See also

[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md),
[`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md)

Other adaptive persistence:
[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md),
[`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md)

## Examples

``` r
dir <- tempfile("pwllm-session-")
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
save_adaptive_session(state, dir, overwrite = TRUE)
restored <- load_adaptive_session(dir)
summarize_adaptive(restored)
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       3               0               0        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
```
