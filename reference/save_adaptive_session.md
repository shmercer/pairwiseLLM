# Save an adaptive session to disk.

Save an adaptive session to disk.

## Usage

``` r
save_adaptive_session(state, session_dir, overwrite = FALSE)
```

## Arguments

- state:

  Adaptive state.

- session_dir:

  Directory to write session artifacts.

- overwrite:

  Logical; overwrite existing artifacts.

## Value

The `session_dir` path, invisibly.

## Details

Saves canonical Adaptive artifacts under `session_dir`: `state.rds`,
`step_log.rds`, `round_log.rds`, `metadata.rds`, optional `btl_fit.rds`,
and optional per-refit item log files when
`state$config$persist_item_log` is `TRUE`. Writes are atomic at file
level to reduce partial-write risk. Persisted `step_log`/`round_log`
files keep the full canonical schemas, so resume preserves expanded
audit fields without recomputation.

## See also

[`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md),
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md)

Other adaptive persistence:
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md),
[`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md)

## Examples

``` r
dir <- tempfile("pwllm-session-")
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
save_adaptive_session(state, dir, overwrite = TRUE)
```
