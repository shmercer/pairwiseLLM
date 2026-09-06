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
  checkpoint_every_steps = NULL,
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

  Optional named list overriding adaptive controller behavior. Unknown
  fields and invalid values abort with an actionable error. See
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
  for the full list of supported keys, detailed semantics, and defaults.

- btl_config:

  Optional named list overriding BTL refit cadence, stopping thresholds,
  and selected round-log diagnostics. Supported fields:

  `refit_pairs_target`

  :   Minimum new committed comparisons required before the next BTL
      refit. Default is `ceiling(N / 2)` clamped to `[20L, 5000L]`
      (Phase A linking uses the active set size).

  `model_variant`

  :   BTL MCMC variant: `"btl"`, `"btl_e"`, `"btl_b"`, or `"btl_e_b"`.
      Default is `"btl_e_b"`.

  `ess_bulk_min`

  :   Minimum bulk ESS required for diagnostics to pass. Default is
      `max(400, round(20 * sqrt(N)))`.

  `ess_bulk_min_near_stop`

  :   Stricter ESS requirement when a run is close to stopping. Default
      is `max(1000, round(50 * sqrt(N)))`.

  `max_rhat`

  :   Maximum allowed split-\\\hat{R}\\ diagnostic value. Default is
      `1.01`.

  `divergences_max`

  :   Maximum allowed divergent transitions. Default is `0L`.

  `eap_reliability_min`

  :   Minimum EAP reliability to allow stopping. Default is `0.90`.

  `stability_lag`

  :   Lag (in refits) used for stability checks. Default is `2L`.

  `theta_corr_min`

  :   Minimum lagged correlation of posterior means. Default is `0.95`.

  `theta_sd_rel_change_max`

  :   Maximum relative change in posterior SD allowed by stability
      checks. Default is `0.10`.

  `rank_spearman_min`

  :   Minimum lagged Spearman rank correlation. Default is `0.95`.

  `near_tie_p_low`, `near_tie_p_high`

  :   Probability band used only for near-tie diagnostics in round
      logging (not used for stopping decisions). Defaults are `0.40` and
      `0.60`.

  `deferred_audit_max_draws`

  :   Maximum posterior draws used for report-only deferred round-log
      audit metrics such as near-tie and credible-interval width
      summaries. This does not affect CmdStan diagnostics or stop gates.
      Default is `400L`; use `Inf` to use all draws.

  `phase_b_refit_parallel`, `phase_b_refit_workers`

  :   Opt-in parallel execution for spoke-separable Phase B post-refit
      updates after the main BTL refit. Uses forked local workers and is
      only supported on Unix-like platforms. Defaults are `FALSE` and
      `1L`.

  Defaults are resolved from the current item count `N`, then merged
  with user overrides.

- session_dir:

  Optional directory for saving session artifacts. If `NULL`, uses
  `state$config$session_dir`. Default is `NULL`.

- persist_item_log:

  Logical; when TRUE, write per-refit item logs to disk. If `NULL`, uses
  `state$config$persist_item_log`. Default is `NULL`.

- checkpoint_every_steps:

  Optional positive integer checkpoint cadence for ordinary live
  persistence. If `NULL`, uses the persisted state value when present,
  otherwise defaults to `100L`.

- progress:

  Progress output: `"all"`, `"refits"`, `"steps"`, or `"none"`. Default
  is `"all"`.

- progress_redraw_every:

  Redraw progress bar every N steps. Default is `10L`.

- progress_show_events:

  Logical; when TRUE, print notable step events. Default is `TRUE`.

- progress_errors:

  Logical; when TRUE, include invalid-step events. Default is `TRUE`.

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

Within-set routing is TrueSkill-based with utility \$\$U_0 = p\_{ij}(1 -
p\_{ij})\$\$. After an accepted posterior refit is available, the
long-link gate uses the BTL posterior win probability for candidate
eligibility; before that it falls back deterministically to TrueSkill.
In linking Phase B, anchor/strata routing uses linking-global scores
built from Phase A summaries and the accepted anchored-joint state.
Linking Phase B routing ranks eligible cross-set candidates by
ridge-stabilized D-optimal log-det information gain on the active
linking parameter block using order-averaged Model D probabilities.
Linking inference parameters remain inference-only (diagnostics and
stopping) and are not direct pair-selection objectives. Phase B uses a
hard-lock hub-fixed fit and a deterministic accepted state before the
first linking refit. Exploration/exploitation routing and fallback
handling are recorded in `step_log`.

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
quotas, while BTL remains inference-only. If
`adaptive_config$max_pairs_after_stop > 0`, the run records a stop
boundary at the first refit with `stop_decision = TRUE` and allows at
most that many additional committed comparisons before deterministic
termination. Round logs record `max_pairs_after_stop` and
`pairs_committed_after_stop`.

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
#> # A tibble: 6 × 97
#>   step_id timestamp           pair_id     i     j i_id  j_id      A     B A_id 
#>     <int> <dttm>                <int> <int> <int> <chr> <chr> <int> <int> <chr>
#> 1       1 2026-09-06 22:06:41       1     1     5 S01   S05       5     1 S05  
#> 2       2 2026-09-06 22:06:42       2     5     8 S05   S08       8     5 S08  
#> 3       3 2026-09-06 22:06:42       3     8     6 S08   S06       6     8 S06  
#> 4       4 2026-09-06 22:06:42       4     6     2 S06   S02       2     6 S02  
#> 5       5 2026-09-06 22:06:42       5     2     4 S02   S04       4     2 S04  
#> 6       6 2026-09-06 22:06:42       6     4     3 S04   S03       3     4 S03  
#> # ℹ 87 more variables: B_id <chr>, unordered_key <chr>, ordered_key <chr>,
#> #   Y <int>, status <chr>, judge_backend <chr>, judge_model <chr>,
#> #   judge_endpoint <chr>, judge_valid <lgl>, judge_invalid_reason <chr>,
#> #   llm_status_code <int>, llm_error_message <chr>, llm_custom_id <chr>,
#> #   prompt_tokens <dbl>, completion_tokens <dbl>, total_tokens <dbl>,
#> #   raw_response_json <chr>, round_id <int>, round_stage <chr>,
#> #   pair_type <chr>, used_in_round_i <int>, used_in_round_j <int>, …
names(logs)
#> [1] "step_log"       "round_log"      "item_log"       "link_stage_log"

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
