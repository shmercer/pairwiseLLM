# Validate an adaptive session directory.

Validate an adaptive session directory.

## Usage

``` r
validate_session_dir(session_dir)
```

## Arguments

- session_dir:

  Directory containing session artifacts.

## Value

A metadata list containing at least `schema_version`, `package_version`,
`n_items`, `warm_start_mode`, and `pairing_strategy`.

## Details

Verifies that required session artifacts exist and that serialized logs
match canonical schemas for `step_log` and `round_log`. This check is
intended as a preflight for
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md)
and enforces the canonical adaptive session metadata shape. Validation
is strict: known legacy fields are backfilled before checking canonical
columns and types. Other added/removed/reordered columns abort
validation. Saved mode/strategy metadata must agree with the
authoritative state.

## See also

[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md),
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md)

Other adaptive persistence:
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md),
[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md)

## Examples

``` r
dir <- tempfile("pwllm-session-")
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
save_adaptive_session(state, dir, overwrite = TRUE)
validate_session_dir(dir)
#> $schema_version
#> [1] "adaptive-session"
#> 
#> $package_version
#> [1] "1.4.0"
#> 
#> $n_items
#> [1] 3
#> 
#> $predictive_prior_digest
#> NULL
#> 
#> $warm_start_mode
#> [1] "cold"
#> 
#> $pairing_strategy
#> [1] "hybrid"
#> 
```
