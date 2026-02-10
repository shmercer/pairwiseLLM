# Summarize adaptive refits

Build a thin per-refit diagnostics summary from the adaptive round log.
This is a pure view over `round_log` and does not recompute posterior
quantities or stop metrics.

## Usage

``` r
summarize_refits(state, last_n = NULL, include_optional = TRUE)
```

## Arguments

- state:

  An `adaptive_state` or list containing adaptive logs.

- last_n:

  Optional positive integer; return only the last `n` rows.

- include_optional:

  Logical; include optional diagnostic columns.

## Value

A tibble with one row per refit (canonical `round_log` schema).

## Details

The round log is the canonical stop-audit trail. This summary is a
direct view over `round_log` with no recomputation.

Key fields include:

- identity: `refit_id`, `round_id_at_refit`, `step_id_at_refit`

- run scale: `total_pairs_done`, `new_pairs_since_last_refit`,
  `n_unique_pairs_seen`

- candidate health: `proposed_pairs_mode`,
  `starve_rate_since_last_refit`, `fallback_rate_since_last_refit`,
  `fallback_used_mode`, `starvation_reason_mode`

- identifiability/quota adaptation: `global_identified`,
  `global_identified_reliability_min`,
  `global_identified_rank_corr_min`, `long_quota_raw`,
  `long_quota_effective`, `long_quota_removed`, `realloc_to_mid`,
  `realloc_to_local`

- diagnostics/stopping: `diagnostics_pass`, `divergences`, `max_rhat`,
  `min_ess_bulk`, `ess_bulk_required`, `reliability_EAP`, `rho_theta`,
  `delta_sd_theta`, `rho_rank`, `stop_decision`, `stop_reason`

- report-only uncertainty metrics: `ci95_theta_width_*`,
  `near_tie_adj_*`, `cov_trace_theta`, `top20_boundary_entropy_*`,
  `nn_diff_sd_*`

## Examples

``` r
# These summaries work on either an adaptive_state or a plain list of logs.
logs <- list(
  round_log = tibble::tibble(
    refit_id = 1:2,
    round_id_at_refit = c(1L, 2L),
    step_id_at_refit = c(10L, 20L),
    new_pairs_since_last_refit = c(50L, 50L),
    total_pairs_done = c(50L, 100L),
    divergences = c(0L, 0L),
    max_rhat = c(1.01, 1.00),
    min_ess_bulk = c(800, 900),
    stop_decision = c(NA, TRUE),
    stop_reason = c(NA_character_, "btl_converged")
  )
)

# Full per-refit view:
summarize_refits(logs)
#> # A tibble: 2 × 10
#>   refit_id round_id_at_refit step_id_at_refit new_pairs_since_last_refit
#>      <int>             <int>            <int>                      <int>
#> 1        1                 1               10                         50
#> 2        2                 2               20                         50
#> # ℹ 6 more variables: total_pairs_done <int>, divergences <int>,
#> #   max_rhat <dbl>, min_ess_bulk <dbl>, stop_decision <lgl>, stop_reason <chr>

# Only the most recent refit row:
summarize_refits(logs, last_n = 1)
#> # A tibble: 1 × 10
#>   refit_id round_id_at_refit step_id_at_refit new_pairs_since_last_refit
#>      <int>             <int>            <int>                      <int>
#> 1        2                 2               20                         50
#> # ℹ 6 more variables: total_pairs_done <int>, divergences <int>,
#> #   max_rhat <dbl>, min_ess_bulk <dbl>, stop_decision <lgl>, stop_reason <chr>

# Drop optional diagnostics if you want a compact core summary:
summarize_refits(logs, include_optional = FALSE)
#> # A tibble: 2 × 10
#>   refit_id round_id_at_refit step_id_at_refit total_pairs_done
#>      <int>             <int>            <int>            <int>
#> 1        1                 1               10               50
#> 2        2                 2               20              100
#> # ℹ 6 more variables: new_pairs_since_last_refit <int>, divergences <int>,
#> #   max_rhat <dbl>, min_ess_bulk <dbl>, stop_decision <lgl>, stop_reason <chr>
```
