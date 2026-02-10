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
