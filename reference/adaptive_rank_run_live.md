# Adaptive ranking live runner

Execute stepwise adaptive ranking with a user-supplied judge.

## Usage

``` r
adaptive_rank_run_live(
  state,
  judge,
  n_steps = 1L,
  fit_fn = NULL,
  adaptive_config = NULL,
  btl_config = NULL,
  session_dir = NULL,
  persist_item_log = NULL,
  progress = c("all", "refits", "steps", "none"),
  progress_redraw_every = 10L,
  progress_show_events = TRUE,
  progress_errors = TRUE,
  ...
)
```

## Arguments

- state:

  An adaptive state object created by
  [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md).

- judge:

  A function called as `judge(A, B, state, ...)` that returns a list
  with `is_valid = TRUE` and `Y` in `0/1`, or `is_valid = FALSE` with
  `invalid_reason`.

- n_steps:

  Maximum number of attempted adaptive steps to execute in this call.
  The run may terminate earlier if candidate starvation is encountered
  or if BTL stopping criteria are met at a refit. Each attempted step
  counts toward this budget, including invalid judge responses.

- fit_fn:

  Optional BTL fit function for deterministic testing; defaults to
  `default_btl_fit_fn()` when a refit is due.

- adaptive_config:

  Optional named list overriding adaptive controller behavior. Supported
  fields: `global_identified_reliability_min`,
  `global_identified_rank_corr_min`, `p_long_low`, `p_long_high`,
  `long_taper_mult`, `long_frac_floor`, `mid_bonus_frac`,
  `explore_taper_mult`, `boundary_k`, `boundary_window`,
  `boundary_frac`, `p_star_override_margin`, and
  `star_override_budget_per_round`. Unknown fields and invalid values
  abort with an actionable error.

- btl_config:

  Optional named list overriding BTL refit cadence, stopping thresholds,
  and selected round-log diagnostics. Supported fields:

  `refit_pairs_target`

  :   Minimum new committed comparisons required before the next BTL
      refit.

  `model_variant`

  :   BTL MCMC variant: `"btl"`, `"btl_e"`, `"btl_b"`, or `"btl_e_b"`.

  `ess_bulk_min`

  :   Minimum bulk ESS required for diagnostics to pass.

  `ess_bulk_min_near_stop`

  :   Stricter ESS requirement when a run is close to stopping.

  `max_rhat`

  :   Maximum allowed split-\\\hat{R}\\ diagnostic value.

  `divergences_max`

  :   Maximum allowed divergent transitions.

  `eap_reliability_min`

  :   Minimum EAP reliability to allow stopping.

  `stability_lag`

  :   Lag (in refits) used for stability checks.

  `theta_corr_min`

  :   Minimum lagged correlation of posterior means.

  `theta_sd_rel_change_max`

  :   Maximum relative change in posterior SD allowed by stability
      checks.

  `rank_spearman_min`

  :   Minimum lagged Spearman rank correlation.

  `near_tie_p_low`, `near_tie_p_high`

  :   Probability band used only for near-tie diagnostics in round
      logging (not used for stopping decisions).

  Defaults are resolved from the current item count, then merged with
  user overrides.

- session_dir:

  Optional directory for saving session artifacts.

- persist_item_log:

  Logical; when TRUE, write per-refit item logs to disk.

- progress:

  Progress output: "all", "refits", "steps", or "none".

- progress_redraw_every:

  Redraw progress bar every N steps.

- progress_show_events:

  Logical; when TRUE, print notable step events.

- progress_errors:

  Logical; when TRUE, include invalid-step events.

- ...:

  Additional arguments passed through to `judge()`.

## Value

An updated `adaptive_state`. The returned state includes appended
`step_log` rows for attempted steps and, when refits occur, appended
`round_log` and `item_log` entries.

## Details

Each iteration attempts at most one pair evaluation ("one-pair step"),
then applies transactional updates if and only if the judge response is
valid. Invalid responses produce a logged step with `pair_id = NA` and
must not update committed-comparison state.

Pair selection is TrueSkill-based and does not use BTL posterior draws.
Utility is based on \$\$U_0 = p\_{ij}(1 - p\_{ij})\$\$ with
exploration/exploitation routing and fallback handling recorded in
`step_log`.

Round scheduling uses stage-specific admissibility:

- rolling-anchor links compare one anchor and one non-anchor endpoint;

- long/mid links exclude anchor endpoints and enforce stratum-distance
  bounds;

- local-link routing admits same-stratum pairs and anchor-involving
  pairs within local stage bounds.

Exposure and repeat handling are soft, stage-local constraints:
under-represented exploration uses degree set `deg <= D_min + 1`, while
repeat-pressure gating uses bottom-quantile `recent_deg` (default
quantile `0.25`) and per-endpoint repeat-slot accounting against
`repeat_in_round_budget`.

Top-band defaults for stratum construction are `top_band_pct = 0.10` and
`top_band_bins = 5`, with top-band size `ceiling(top_band_pct * N)`.

Bayesian BTL refits are triggered on step-based cadence and evaluated
with diagnostics gates (including ESS thresholds), reliability, and
lagged stability criteria. Refit-level outcomes are appended to
`round_log`; per-item posterior summaries are appended to `item_log`.
Controller behavior can change after refits via identifiability-gated
settings in `adaptive_config`; those controls affect pair routing and
quotas, while BTL remains inference-only.

## See also

[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

## Examples

``` r
# ------------------------------------------------------------------
# Offline end-to-end workflow (fast, deterministic, CRAN-safe)
# ------------------------------------------------------------------
data("example_writing_samples", package = "pairwiseLLM")

items <- dplyr::rename(
  example_writing_samples[1:8, c("ID", "text", "quality_score")],
  item_id = ID
)

# Use the package defaults for trait and prompt template.
trait <- trait_description("overall_quality")
prompt_template <- set_prompt_template()

# Deterministic local judge based on fixture quality scores.
sim_judge <- function(A, B, state, ...) {
  y <- as.integer(A$quality_score[[1]] >= B$quality_score[[1]])
  list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
}

session_dir <- tempfile("pwllm-adaptive-session-")

state <- adaptive_rank_start(
  items = items,
  seed = 42,
  adaptive_config = list(
    global_identified_reliability_min = 0.85,
    star_override_budget_per_round = 2L
  ),
  session_dir = session_dir,
  persist_item_log = TRUE
)

state <- adaptive_rank_run_live(
  state = state,
  judge = sim_judge,
  n_steps = 6,
  btl_config = list(
    # Keep examples lightweight while showing custom stop config inputs.
    refit_pairs_target = 50L,
    ess_bulk_min = 400,
    eap_reliability_min = 0.90
  ),
  adaptive_config = list(
    explore_taper_mult = 0.40,
    boundary_frac = 0.20
  ),
  progress = "steps",
  progress_redraw_every = 1L,
  progress_show_events = TRUE,
  progress_errors = TRUE
)
#> step 1: new_pairs_since_last_refit=1/50 committed=1 invalid=0 starved=0
#> step 2: new_pairs_since_last_refit=2/50 committed=2 invalid=0 starved=0
#> step 3: new_pairs_since_last_refit=3/50 committed=3 invalid=0 starved=0
#> step 4: new_pairs_since_last_refit=4/50 committed=4 invalid=0 starved=0
#> step 5: new_pairs_since_last_refit=5/50 committed=5 invalid=0 starved=0
#> step 6: new_pairs_since_last_refit=6/50 committed=6 invalid=0 starved=0

# Print and inspect run outputs.
print(state)
#> Adaptive state
#> items: 8
#> steps: 6 (committed=6)
#> refits: 0
#> last stop: continue
run_summary <- summarize_adaptive(state)
step_view <- adaptive_step_log(state)
logs <- adaptive_get_logs(state)

run_summary
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       8               6               6        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
head(step_view)
#> # A tibble: 6 × 51
#>   step_id timestamp           pair_id     i     j     A     B     Y status
#>     <int> <dttm>                <int> <int> <int> <int> <int> <int> <chr> 
#> 1       1 2026-02-10 23:43:44       1     1     5     1     5     0 ok    
#> 2       2 2026-02-10 23:43:44       2     5     8     5     8     0 ok    
#> 3       3 2026-02-10 23:43:44       3     8     6     8     6     1 ok    
#> 4       4 2026-02-10 23:43:44       4     6     2     6     2     1 ok    
#> 5       5 2026-02-10 23:43:44       5     2     4     2     4     0 ok    
#> 6       6 2026-02-10 23:43:44       6     4     3     4     3     1 ok    
#> # ℹ 42 more variables: round_id <int>, round_stage <chr>, pair_type <chr>,
#> #   used_in_round_i <int>, used_in_round_j <int>, is_anchor_i <lgl>,
#> #   is_anchor_j <lgl>, stratum_i <int>, stratum_j <int>, dist_stratum <int>,
#> #   stage_committed_so_far <int>, stage_quota <int>, is_explore_step <lgl>,
#> #   explore_mode <chr>, explore_reason <chr>, explore_rate_used <dbl>,
#> #   local_priority_mode <chr>, long_gate_pass <lgl>, long_gate_reason <chr>,
#> #   star_override_used <lgl>, star_override_reason <chr>, …
names(logs)
#> [1] "step_log"  "round_log" "item_log" 

# Resume from disk and continue.
resumed <- adaptive_rank_resume(session_dir)
resumed <- adaptive_rank_run_live(
  state = resumed,
  judge = sim_judge,
  n_steps = 4,
  progress = "none"
)
summarize_adaptive(resumed)
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       8              10               9        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>

# ------------------------------------------------------------------
# Live OpenAI workflow via backend-agnostic llm_compare_pair()
# ------------------------------------------------------------------
if (FALSE) { # \dontrun{
# Requires network + OPENAI_API_KEY. This incurs API cost.
# check_llm_api_keys() is a quick preflight.
check_llm_api_keys()

data("example_writing_samples", package = "pairwiseLLM")
live_items <- dplyr::rename(
  example_writing_samples[1:12, c("ID", "text")],
  item_id = ID
)

# Default trait/template setup used by the backend-agnostic runner.
trait <- trait_description("overall_quality")
prompt_template <- set_prompt_template()

live_session_dir <- file.path(tempdir(), "pwllm-adaptive-openai")

judge_openai <- function(A, B, state, ...) {
  res <- llm_compare_pair(
    ID1 = A$item_id[[1]],
    text1 = A$text[[1]],
    ID2 = B$item_id[[1]],
    text2 = B$text[[1]],
    model = "gpt-5.1",
    trait_name = trait$name,
    trait_description = trait$description,
    prompt_template = prompt_template,
    backend = "openai",
    endpoint = "responses",
    reasoning = "low",
    service_tier = "flex",
    include_thoughts = FALSE,
    temperature = NULL,
    top_p = NULL,
    logprobs = NULL
  )

  better_id <- res$better_id[[1]]
  ok_ids <- c(A$item_id[[1]], B$item_id[[1]])
  if (is.na(better_id) || !(better_id %in% ok_ids)) {
    return(list(
      is_valid = FALSE,
      Y = NA_integer_,
      invalid_reason = "model_response_invalid"
    ))
  }

  list(
    is_valid = TRUE,
    Y = as.integer(identical(better_id, A$item_id[[1]])),
    invalid_reason = NA_character_
  )
}

state_live <- adaptive_rank_start(
  items = live_items,
  seed = 2026,
  session_dir = live_session_dir,
  persist_item_log = TRUE
)

state_live <- adaptive_rank_run_live(
  state = state_live,
  judge = judge_openai,
  n_steps = 120L,
  btl_config = list(
    refit_pairs_target = 20L,
    ess_bulk_min = 500,
    ess_bulk_min_near_stop = 1200,
    max_rhat = 1.01,
    divergences_max = 0L,
    eap_reliability_min = 0.92,
    stability_lag = 2L,
    theta_corr_min = 0.97,
    theta_sd_rel_change_max = 0.08,
    rank_spearman_min = 0.97
  ),
  progress = "all",
  progress_redraw_every = 1L,
  progress_show_events = TRUE,
  progress_errors = TRUE
)

# Reporting outputs for end users.
print(state_live)
run_summary <- summarize_adaptive(state_live)
refit_summary <- summarize_refits(state_live)
item_summary <- summarize_items(state_live)
logs <- adaptive_get_logs(state_live)

# Store outputs for audit/reproducibility.
saveRDS(
  list(
    run_summary = run_summary,
    refit_summary = refit_summary,
    item_summary = item_summary,
    logs = logs
  ),
  file.path(live_session_dir, "adaptive_outputs.rds")
)

# Resume from stored state and continue sampling.
state_live <- adaptive_rank_resume(live_session_dir)
state_live <- adaptive_rank_run_live(
  state = state_live,
  judge = judge_openai,
  n_steps = 40L,
  progress = "refits"
)
print(summarize_adaptive(state_live))
} # }
```
