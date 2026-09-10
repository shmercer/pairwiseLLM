# Run adaptive ranking end-to-end from data and model settings

High-level workflow wrapper that reads sample data, constructs an LLM
judge, starts or resumes adaptive state, runs
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
and returns state plus summary outputs.

## Usage

``` r
adaptive_rank(
  data,
  id_col = 1,
  text_col = 2,
  backend = c("openai", "anthropic", "gemini", "vertex", "together", "ollama"),
  model = NULL,
  trait = "overall_quality",
  trait_name = NULL,
  trait_description = NULL,
  prompt_template = set_prompt_template(),
  endpoint = "chat.completions",
  api_key = NULL,
  include_raw = FALSE,
  judge_args = list(),
  judge_call_args = list(),
  n_steps = 1L,
  fit_fn = NULL,
  adaptive_config = NULL,
  btl_config = NULL,
  session_dir = NULL,
  persist_item_log = FALSE,
  checkpoint_every_steps = NULL,
  resume = TRUE,
  seed = 1L,
  progress = c("all", "refits", "steps", "none"),
  progress_redraw_every = 10L,
  progress_show_events = TRUE,
  progress_errors = TRUE,
  save_outputs = FALSE,
  output_file = NULL,
  judge = NULL,
  warm_start_model = NULL,
  warm_start_prior = NULL,
  warm_start_features = NULL,
  warm_start_python = NULL,
  warm_start_prior_sd = NULL
)
```

## Arguments

- data:

  Data source: a data frame/tibble, a file path (`.csv`, `.tsv`, `.txt`,
  `.rds`), or a directory containing `.txt` files.

- id_col:

  ID column selector for tabular inputs. Passed to
  [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md).
  Default is `1`.

- text_col:

  Text column selector for tabular inputs. Passed to
  [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md).
  Default is `2`.

- backend:

  Backend passed to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).
  Choices are `"openai"`, `"anthropic"`, `"gemini"`, `"vertex"`,
  `"together"`, and `"ollama"`. Default is `"openai"`.

- model:

  Model passed to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).
  Required when `judge` is `NULL`. Default is `NULL`.

- trait:

  Built-in trait key used when no custom trait is supplied. Ignored when
  both `trait_name` and `trait_description` are supplied. Default is
  `"overall_quality"`.

- trait_name:

  Optional custom trait display name.

- trait_description:

  Optional custom trait definition.

- prompt_template:

  Prompt template string. Defaults to
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- endpoint:

  Endpoint family passed to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).
  Only used when `backend = "openai"`; choices are `"chat.completions"`
  and `"responses"`. Default is `"chat.completions"`. Ignored for other
  backends.

- api_key:

  Optional API key passed to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).
  Default is `NULL`.

- include_raw:

  Logical; forwarded to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).
  Default is `FALSE`.

- judge_args:

  Named list of fixed additional arguments forwarded to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
  by the generated judge. Default is
  [`list()`](https://rdrr.io/r/base/list.html).

- judge_call_args:

  Named list of additional arguments forwarded to the judge at run time
  through
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).
  Default is [`list()`](https://rdrr.io/r/base/list.html).

- n_steps:

  Maximum number of attempted adaptive steps to execute in this call.
  The run may return earlier due to candidate starvation or BTL stop
  criteria. Attempted invalid steps also count toward this limit.

- fit_fn:

  Optional fit override passed to
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).

- adaptive_config:

  Optional named list of adaptive controller overrides. Unknown fields
  and invalid values abort with actionable errors.

  Supported keys (with defaults) include:

  `global_identified_reliability_min`

  :   Global EAP reliability threshold used to mark the run as globally
      identified after a refit. Default is `0.80`.

  `global_identified_rank_corr_min`

  :   Minimum Spearman correlation between the TrueSkill rank proxy and
      the BTL posterior mean ranks used to mark the run as globally
      identified after a refit. Default is `0.90`.

  `p_long_low`

  :   Lower bound for long-link posterior win probability gating after
      global identifiability when an accepted posterior refit is
      available. Before posterior availability, the gate falls back
      deterministically to TrueSkill. Default is `0.10`.

  `p_long_high`

  :   Upper bound for long-link posterior win probability gating after
      global identifiability when an accepted posterior refit is
      available. Before posterior availability, the gate falls back
      deterministically to TrueSkill. Default is `0.90`.

  `long_taper_mult`

  :   Multiplier controlling long-link quota tapering after global
      identifiability. Default is `0.25`.

  `long_frac_floor`

  :   Floor fraction for long-link quota after tapering. Default is
      `0.02`.

  `mid_bonus_frac`

  :   Fraction of tapered long-link quota reallocated to mid-links.
      Default is `0.20`.

  `explore_taper_mult`

  :   Multiplier controlling exploration tapering after global
      identifiability. Default is `0.50`.

  `boundary_k`

  :   Top/bottom band size used by boundary-priority routing after
      global identifiability. Default is `20L`.

  `boundary_window`

  :   Lookback window (steps) used by boundary-priority routing after
      global identifiability. Default is `max(10L, ceiling(0.05 * N))`
      where `N` is the number of items.

  `boundary_frac`

  :   Fraction of local-stage steps eligible for boundary-priority
      routing after global identifiability. Default is `0.15`.

  `p_star_override_margin`

  :   Near-tie probability margin for star-cap override consideration.
      Default is `0.05`.

  `star_override_budget_per_round`

  :   Per-round budget of star-cap overrides allowed by the near-tie
      rule. Default is `1L`.

  `run_mode`

  :   Run mode. Choices are `"within_set"` (single-set),
      `"link_one_spoke"` (hub + one spoke), and `"link_multi_spoke"`
      (hub + multiple spokes). Default is `"within_set"`. Linking modes
      require multi-set inputs with `set_id` and `global_item_id` in
      `data`.

  `hub_id`

  :   Hub `set_id` for linking modes. Default is `1L`.

  Phase B estimation

  :   Linking modes use anchored-joint estimation with a hard-locked
      hub, global-shared judge parameters, concurrent spokes, and
      fail-fast Phase A artifact import. Historical transform/free-lock
      config fields are normalized only when loading older sessions or
      Phase A artifacts; they are not accepted as new `adaptive_config`
      keys.

  `anchored_joint_spoke_prior_scale`

  :   Scale multiplier for anchored- joint spoke priors. Default is
      `1.0`.

  `anchored_joint_sd_floor`

  :   Lower bound applied to anchored-joint spoke prior SDs derived from
      Phase A artifacts. Default is `0.02`.

  `anchored_joint_spoke_prior_fallback_sd`

  :   Fallback anchored-joint spoke prior SD used when artifact-level
      SDs are unavailable. Default is `1.0`.

  `link_identified_reliability_min`

  :   Minimum `reliability_link_global` value on the linking-active item
      domain used to mark a spoke as identified. Default is `0.80`.

  `link_stop_reliability_min`

  :   Minimum `reliability_link_global` value on the linking-active item
      domain used to permit linking stop. Default is `0.90`.

  `link_rank_corr_min`

  :   Minimum Spearman rank correlation between TrueSkill and
      transformed BTL posterior mean ranks on the linking-active item
      domain. Default is `0.90`.

  `max_pairs_after_stop`

  :   Stop-boundary budget: when `0L`, the run stops immediately after
      the first refit with `stop_decision = TRUE`. Values `> 0L` allow
      that many additional committed comparisons after the first stop
      boundary before deterministic termination. Default is `0L`.

  `probe_panel_edges`

  :   Optional explicit planned held-out probe target per spoke. When
      omitted in linking modes, the default scales with the largest
      spoke: `max(160L, ceiling(0.12 * max_spoke_items))`. When
      supplied, the value must be a positive integer and becomes the
      canonical planned target recorded in Phase B logs.

  `probe_pairs_per_refit_per_spoke`

  :   Base held-out probe collection cap per spoke per refit window
      while the spoke remains active in Phase B. If omitted in linking
      modes, the default scales with the largest spoke:
      `max(4L, ceiling(0.0035 * max_spoke_items))`. The runtime uses
      this as a fixed per-refit cap and does not apply bootstrap or
      sole-blocker probe acceleration.

  `probe_edges_min_for_stop`

  :   Minimum realized held-out probe edges required before Phase B stop
      or escalation can be evaluated. If omitted in linking modes, the
      default scales with the largest spoke:
      `max(120L, ceiling(0.106 * max_spoke_items))`.

  `probe_near_boundary_min_frac`, `probe_extreme_max_frac`, `probe_midrange_min_frac`, `probe_unique_hub_min_frac`, `probe_unique_spoke_min_frac`, `probe_rank_bins`, `probe_rank_bins_hub_min`, `probe_rank_bins_spoke_min`, `probe_brier_near_boundary_max`, `probe_ece_max`

  :   Held-out probe quality gates used by Phase B stop decisions to
      require useful probability spread, hub/spoke item coverage,
      rank-bin coverage, near-boundary Brier calibration, and
      calibration ECE.

  `probe_brier_delta_min`

  :   Minimum held-out probe Brier improvement required by the Phase B
      probe quality gate. Default is `0.005`.

  `probe_brier_max`

  :   Maximum held-out probe Brier score allowed by the Phase B stop
      gate. Default is `0.19`.

  `probe_pred_rmse_max`

  :   Maximum lagged held-out probe prediction RMSE allowed by the Phase
      B stop gate. Default is `0.015`.

  `theta_global_rmse_max`

  :   Maximum lagged transformed-score RMSE on the direct-evidence spoke
      scope allowed by the Phase B stop gate. Default is `0.05`.

  `stability_window_refits`

  :   Number of eligible refits retained in the rolling stop window.
      Default is `3L`.

  `stability_passes_required`

  :   Minimum number of passing eligible refits required within the
      rolling stop window. Default is `2L`.

  `min_refits_in_phase_b`

  :   Minimum refit index within Phase B before linking stop can be
      evaluated. Default is `3L`.

  `reliability_var_mu_epsilon`

  :   Degeneracy guard for the active-domain variance of posterior
      transformed-score means used in linking reliability. Default is
      `1e-6`.

  `reliability_total_var_epsilon`

  :   Degeneracy guard for the total active-domain transformed-score
      variance used in linking reliability. Default is `1e-6`.

  `hub_anchor_required_phase_b`

  :   Controls the normative `HubEligible` domain used for Phase B
      held-out probe construction. When `TRUE` (default), planned probes
      are drawn from the hub anchor pool; when `FALSE`, they are drawn
      from the full hub set.

  `spoke_quantile_coverage_bins`

  :   Cross-set coverage control: number of quantile bins used to ensure
      spoke items across the score distribution receive cross-set
      exposure within each refit window. Default is `3L`.

  `spoke_quantile_coverage_min_per_bin_per_refit`

  :   Cross-set coverage control: minimum cross-set comparisons per
      quantile bin per refit window. Default is `1L`.

  `min_cross_set_pairs_per_spoke_per_refit`

  :   Only used in concurrent multi-spoke linking. Minimum cross-set
      committed comparisons per spoke per refit window. Default is `5L`.

  `phase_a_mode`

  :   Phase A handling for linking modes. Choices are `"run"` (compute
      within-set Phase A artifacts in-run), `"import"` (require
      user-supplied artifacts), and `"mixed"` (import where provided,
      otherwise run). Default is `"run"`.

  `phase_a_required_reliability_min`

  :   Minimum within-set EAP reliability required for Phase A artifacts
      to be considered ready (unless an imported artifact is explicitly
      marked `quality_gate_accepted = TRUE` as a trusted external
      quality override). Default is `0.80`.

  `phase_a_artifacts`

  :   Named list mapping `set_id` to an imported Phase A artifact (list)
      or a `.rds` path containing one. On the wrapper surface, this
      field also accepts a prior `adaptive_rank()` `phase_a` return, an
      `out$phase_a$manifest`, a saved session directory, or a
      `phase_a_artifacts/` directory, and normalizes those inputs back
      to the canonical named-list form before runtime validation.
      Imported artifacts must match the current normalized BTL
      `model_variant`; all four canonical variants (`"btl"`, `"btl_e"`,
      `"btl_b"`, `"btl_e_b"`) are supported when the artifact and run
      variants match. Default is
      [`list()`](https://rdrr.io/r/base/list.html).

  Wrapper preflight validates linking mode combinations against supplied
  data and aborts early for incompatible `run_mode`/set structure
  combinations.

- btl_config:

  Optional named list passed to
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
  to control BTL refit cadence, stopping diagnostics, and selected
  round-log diagnostics. Supported fields:

  `refit_pairs_target`

  :   Minimum new committed comparisons required before the next BTL
      refit. Default is `ceiling(N / 2)` clamped to `[20L, 5000L]`. In
      linking Phase A, `N` is the active Phase A set size. In concurrent
      linking Phase B, the effective target is raised when needed so
      each active spoke can satisfy the configured active probe floor
      plus the base per-refit probe cap.

  `model_variant`

  :   BTL likelihood variant used for inference only. Choices are
      `"btl"` (no lapse, no position bias), `"btl_e"` (lapse), `"btl_b"`
      (position bias), and `"btl_e_b"` (lapse + position bias). Default
      is `"btl_e_b"`.

  `ess_bulk_min`

  :   Minimum bulk effective sample size required for diagnostics to
      pass. Default is `max(400, round(20 * sqrt(N)))`.

  `ess_bulk_min_near_stop`

  :   Stricter bulk ESS requirement used when a run is close to
      stopping. Default is `max(1000, round(50 * sqrt(N)))`.

  `max_rhat`

  :   Maximum allowed split-\\\\hat{R}\\. Default is `1.01`.

  `divergences_max`

  :   Maximum allowed divergent transitions. Default is `0L`.

  `eap_reliability_min`

  :   Minimum EAP reliability required to permit stopping. Default is
      `0.90`.

  `stability_lag`

  :   Lag (in refits) used for stability checks. Default is `2L`.

  `theta_corr_min`

  :   Minimum lagged correlation of posterior means required by
      stability checks. Default is `0.95`.

  `theta_sd_rel_change_max`

  :   Maximum relative change in posterior SD allowed by stability
      checks. Default is `0.10`.

  `rank_spearman_min`

  :   Minimum lagged Spearman rank correlation required by stability
      checks. Default is `0.95`.

  `near_tie_p_low`

  :   Lower bound of the near-tie probability band used for round
      logging only. Default is `0.40`.

  `near_tie_p_high`

  :   Upper bound of the near-tie probability band used for round
      logging only. Default is `0.60`.

  Defaults depend on the current item count `N` and are merged with user
  overrides.

- session_dir:

  Optional session directory for persistence/resume. Default is `NULL`.

- persist_item_log:

  Logical; write per-refit item logs when `TRUE`. Default is `FALSE`.

- checkpoint_every_steps:

  Optional positive integer checkpoint cadence for ordinary live
  persistence. New sessions default to `100L`; resumed sessions reuse
  the persisted cadence unless overridden.

- resume:

  Logical; when `TRUE` and `session_dir` contains a valid session,
  resume from disk; otherwise initialize a new state. Default is `TRUE`.

- seed:

  Integer seed used when creating a new adaptive state. Default is `1L`.

- progress:

  Progress mode for
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).
  Choices are `"all"`, `"refits"`, `"steps"`, and `"none"`. Default is
  `"all"`.

- progress_redraw_every:

  Redraw interval for progress output. Default is `10L`.

- progress_show_events:

  Logical; show step events. Default is `TRUE`.

- progress_errors:

  Logical; show invalid-step events. Default is `TRUE`.

- save_outputs:

  Logical; when `TRUE`, save returned outputs as `.rds`. Default is
  `FALSE`.

- output_file:

  Optional output `.rds` path. If `NULL` and `save_outputs = TRUE`,
  defaults to `file.path(session_dir, "adaptive_outputs.rds")` when
  `session_dir` is set, otherwise to a temporary file.

- judge:

  Optional prebuilt judge function with contract
  `judge(A, B, state, ...)`. If supplied, model/trait/template options
  are ignored and this function is used directly.

- warm_start_model:

  Optional calibrated model/ensemble, path string, or loader reference
  list (`name`/`source` or `path`). Mutually exclusive with
  `warm_start_prior`. Resolve and predict once when creating an
  assessment.

- warm_start_prior:

  Optional
  [`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md)
  object covering all items. Saved numeric scores are centered within
  each BTL refit scope.

- warm_start_features:

  Optional precomputed feature rows for model input; otherwise use item
  texts. Precomputed prediction needs neither Python nor glmnet.

- warm_start_python:

  Explicit Python interpreter for text extraction only.

- warm_start_prior_sd:

  Optional model-derived raw theta prior SD override; scalar or per-item
  vector, default 0.5. Supplied prior objects retain their SDs.

## Value

A list with:

- state:

  Final `adaptive_state`.

- summary:

  Run-level summary from
  [`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md).

- refits:

  Per-refit summary from
  [`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md).

- items:

  Item summary from
  [`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
  sorted by a usable canonical rank column (`rank_link` for linking runs
  when available, otherwise `rank_raw`).

- logs:

  Canonical logs from
  [`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md).

- phase_a:

  Canonical wrapper-visible Phase A discovery surface with per-set
  status, `artifact_dir`, `artifact_paths`, and a reusable `manifest`
  that can be fed back into a later linking run via
  `adaptive_config$phase_a_artifacts`.

- output_file:

  Saved output path when `save_outputs = TRUE`, otherwise `NULL`.

## Details

This helper is designed for end users who want one entry point for
adaptive runs. It supports:

- data input from a data frame, file (`.csv`, `.tsv`, `.txt`, `.rds`),
  or a directory of `.txt` files;

- model/backend configuration through
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md);

- all adaptive runtime controls exposed by
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md);

- wrapper-visible `phase_a` reuse surfaces (`manifest`, `artifact_dir`,
  and per-set status) for separate-run then later-link workflows;

- resumability via `session_dir` and `resume`;

- optional saving of run outputs to an `.rds` artifact.

Model options: use `judge_args` (fixed) and `judge_call_args` (per-run
overrides) to pass any additional
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
arguments, including provider-specific controls such as `reasoning`,
`service_tier`, `temperature`, `top_p`, `logprobs`, `include_thoughts`,
or `host`.

Adaptive options: all key controls from
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
are available directly: `n_steps`, `fit_fn`, `adaptive_config`,
`btl_config`, `progress`, `progress_redraw_every`,
`progress_show_events`, `progress_errors`, `session_dir`, and
`persist_item_log`. Use `adaptive_config` for identifiability-gated
controller behavior and `btl_config` for inference/diagnostics cadence
only.

Linking run modes: `run_mode = "within_set"` is the single-set workflow.
`run_mode = "link_one_spoke"` and `run_mode = "link_multi_spoke"`
require multi-set input (`set_id`/`global_item_id`), enforce
hub\<-\>spoke routing defaults, and preserve Phase A artifact gating
before Phase B cross-set comparisons begin. Phase B uses anchored-joint
estimation with a hard-locked hub and global-shared judge parameters.
Every wrapper call returns canonical `phase_a` outputs that can be fed
back into a later linking run through
`adaptive_config$phase_a_artifacts`.

Selection semantics: pair selection is TrueSkill-driven in one-pair
transactional steps. Rolling anchors are refreshed from current score
proxies and anchor-link routing compares exactly one anchor endpoint
with one non-anchor endpoint. Long/mid-link routing excludes
anchor-anchor and anchor-non-anchor pairs, while local-link routing
admits same-stratum pairs and anchor-involving pairs according to stage
bounds.

Wrapper-visible defaults include top-band refinement
(`top_band_pct = 0.10`, `top_band_bins = 5`) with top-band size computed
as `ceiling(top_band_pct * N)`.

Exposure and repeat routing: under-represented routing is degree-based
(`deg <= D_min + 1`), while repeat-pressure gating is based on recent
exposure (bottom-quantile `recent_deg` with quantile default `0.25`) and
per-endpoint repeat slot accounting.

Inference separation: BTL refits are used for posterior inference,
diagnostics, stop logic, and the long-link posterior gate after an
accepted refit is available. They are not used to choose the next pair.

Resume behavior: when `resume = TRUE` and `session_dir` already contains
adaptive artifacts, failed session loads abort with an actionable error
instead of starting a fresh run silently.

Predictive priors affect ordinary/within-set BTL estimation. Transform,
anchored-joint, and pooled judge refits keep their existing prior rules;
predictive evidence is not injected again. Initial pairing queues and
selection rules retain their existing meaning. Custom fit functions must
consume `state$predictive_prior` explicitly. Resume uses saved
predictions; omit all warm-start arguments on resume.

## See also

[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)

[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)

Other adaptive ranking:
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

## Examples

``` r
data("example_writing_samples", package = "pairwiseLLM")

out <- adaptive_rank(
  data = example_writing_samples[1:8, c("ID", "text", "quality_score")],
  id_col = "ID",
  text_col = "text",
  model = "gpt-5.1",
  judge = function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1]] >= B$quality_score[[1]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  },
  n_steps = 4,
  progress = "none"
)

out$summary
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       8               4               4        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
head(out$logs$step_log)
#> # A tibble: 4 × 97
#>   step_id timestamp           pair_id     i     j i_id  j_id      A     B A_id 
#>     <int> <dttm>                <int> <int> <int> <chr> <chr> <int> <int> <chr>
#> 1       1 2026-09-10 18:18:07       1     1     4 S01   S04       4     1 S04  
#> 2       2 2026-09-10 18:18:07       2     4     8 S04   S08       8     4 S08  
#> 3       3 2026-09-10 18:18:08       3     8     2 S08   S02       2     8 S02  
#> 4       4 2026-09-10 18:18:08       4     2     6 S02   S06       6     2 S06  
#> # ℹ 87 more variables: B_id <chr>, unordered_key <chr>, ordered_key <chr>,
#> #   Y <int>, status <chr>, judge_backend <chr>, judge_model <chr>,
#> #   judge_endpoint <chr>, judge_valid <lgl>, judge_invalid_reason <chr>,
#> #   llm_status_code <int>, llm_error_message <chr>, llm_custom_id <chr>,
#> #   prompt_tokens <dbl>, completion_tokens <dbl>, total_tokens <dbl>,
#> #   raw_response_json <chr>, round_id <int>, round_stage <chr>,
#> #   pair_type <chr>, used_in_round_i <int>, used_in_round_j <int>, …

if (FALSE) { # \dontrun{
# Live run with OpenAI gpt-5.1 + lower-cost Flex processing.
live <- adaptive_rank(
  data = example_writing_samples[1:12, c("ID", "text")],
  backend = "openai",
  model = "gpt-5.1",
  endpoint = "responses",
  judge_args = list(
    reasoning = "low",
    service_tier = "flex",
    include_thoughts = FALSE
  ),
  btl_config = list(
    refit_pairs_target = 20L,
    ess_bulk_min = 500,
    eap_reliability_min = 0.92
  ),
  adaptive_config = list(
    explore_taper_mult = 0.40,
    star_override_budget_per_round = 2L
  ),
  n_steps = 120,
  session_dir = file.path(tempdir(), "adaptive-live"),
  persist_item_log = TRUE,
  resume = TRUE,
  progress = "all",
  save_outputs = TRUE
)

print(live$state)
live$summary

# Wrapper-driven linking workflow (hub + one spoke).
linking_samples <- example_writing_samples[1:12, c("ID", "text")]
linking_samples$set_id <- rep(c(1L, 2L), each = 6L)
linking_samples$global_item_id <- paste0("g_", linking_samples$ID)

link_out <- adaptive_rank(
  data = linking_samples,
  id_col = "ID",
  text_col = "text",
  backend = "openai",
  model = "gpt-5.1",
  adaptive_config = list(
    run_mode = "link_one_spoke",
    hub_id = 1L,
    phase_a_mode = "run",
    probe_panel_edges = 48L,
    hub_anchor_required_phase_b = TRUE,
    max_pairs_after_stop = 0L
  ),
  n_steps = 200,
  session_dir = file.path(tempdir(), "adaptive-link"),
  resume = TRUE,
  progress = "refits"
)

# Later linking from prior wrapper outputs:
# hub_run <- adaptive_rank(
#   data = linking_samples[linking_samples$set_id == 1L, c("ID", "text")],
#   backend = "openai",
#   model = "gpt-5.1",
#   n_steps = 120,
#   progress = "none"
# )
# spoke_run <- adaptive_rank(
#   data = linking_samples[linking_samples$set_id == 2L, c("ID", "text")],
#   backend = "openai",
#   model = "gpt-5.1",
#   n_steps = 120,
#   progress = "none"
# )
#
# link_out <- adaptive_rank(
#   data = linking_samples,
#   id_col = "ID",
#   text_col = "text",
#   backend = "openai",
#   model = "gpt-5.1",
#   adaptive_config = list(
#     run_mode = "link_one_spoke",
#     hub_id = 1L,
#     phase_a_mode = "import",
#     phase_a_artifacts = list(
#       `1` = hub_run$phase_a$manifest,
#       `2` = spoke_run$phase_a$artifact_dir
#     )
#   ),
#   n_steps = 200,
#   progress = "refits"
# )

names(link_out$logs)
} # }
```
