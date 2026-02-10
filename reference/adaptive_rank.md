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
  backend = c("openai", "anthropic", "gemini", "together", "ollama"),
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
  resume = TRUE,
  seed = 1L,
  progress = c("all", "refits", "steps", "none"),
  progress_redraw_every = 10L,
  progress_show_events = TRUE,
  progress_errors = TRUE,
  save_outputs = FALSE,
  output_file = NULL,
  judge = NULL
)
```

## Arguments

- data:

  Data source: a data frame/tibble, a file path (`.csv`, `.tsv`, `.txt`,
  `.rds`), or a directory containing `.txt` files.

- id_col:

  ID column selector for tabular inputs. Passed to
  [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md).

- text_col:

  Text column selector for tabular inputs. Passed to
  [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md).

- backend:

  Backend passed to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).

- model:

  Model passed to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).

- trait:

  Built-in trait key used when no custom trait is supplied. Ignored when
  both `trait_name` and `trait_description` are supplied.

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
  Only used when `backend = "openai"`; ignored otherwise.

- api_key:

  Optional API key passed to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).

- include_raw:

  Logical; forwarded to
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md).

- judge_args:

  Named list of fixed additional arguments forwarded to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
  by the generated judge.

- judge_call_args:

  Named list of additional arguments forwarded to the judge at run time
  through
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).

- n_steps:

  Maximum number of attempted adaptive steps to execute in this call.
  The run may return earlier due to candidate starvation or BTL stop
  criteria. Attempted invalid steps also count toward this limit.

- fit_fn:

  Optional fit override passed to
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).

- adaptive_config:

  Optional named list passed to
  [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md)
  and
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
  to control adaptive controller behavior. Supported fields:
  `global_identified_reliability_min`,
  `global_identified_rank_corr_min`, `p_long_low`, `p_long_high`,
  `long_taper_mult`, `long_frac_floor`, `mid_bonus_frac`,
  `explore_taper_mult`, `boundary_k`, `boundary_window`,
  `boundary_frac`, `p_star_override_margin`, and
  `star_override_budget_per_round`. Unknown fields and invalid values
  abort with actionable errors.

- btl_config:

  Optional named list passed to
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
  to control BTL refit cadence, stopping diagnostics, and selected
  round-log diagnostics. Supported fields: `refit_pairs_target`,
  `model_variant`, `ess_bulk_min`, `ess_bulk_min_near_stop`, `max_rhat`,
  `divergences_max`, `eap_reliability_min`, `stability_lag`,
  `theta_corr_min`, `theta_sd_rel_change_max`, `rank_spearman_min`,
  `near_tie_p_low`, and `near_tie_p_high` (`near_tie_*` affects round
  logging only, not stop decisions). Defaults are resolved from the
  current item count and merged with user overrides.

- session_dir:

  Optional session directory for persistence/resume.

- persist_item_log:

  Logical; write per-refit item logs when `TRUE`.

- resume:

  Logical; when `TRUE` and `session_dir` contains a valid session,
  resume from disk; otherwise initialize a new state.

- seed:

  Integer seed used when creating a new adaptive state.

- progress:

  Progress mode for
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).

- progress_redraw_every:

  Redraw interval for progress output.

- progress_show_events:

  Logical; show step events.

- progress_errors:

  Logical; show invalid-step events.

- save_outputs:

  Logical; when `TRUE`, save returned outputs as `.rds`.

- output_file:

  Optional output `.rds` path. If `NULL` and `save_outputs = TRUE`,
  defaults to `file.path(session_dir, "adaptive_outputs.rds")` when
  `session_dir` is set, otherwise to a temporary file.

- judge:

  Optional prebuilt judge function with contract
  `judge(A, B, state, ...)`. If supplied, model/trait/template options
  are ignored and this function is used directly.

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
  [`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md).

- logs:

  Canonical logs from
  [`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md).

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
diagnostics, and stopping only. They are not used to choose the next
pair.

Resume behavior: when `resume = TRUE` and `session_dir` already contains
adaptive artifacts, failed session loads abort with an actionable error
instead of starting a fresh run silently.

## See also

[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)

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
#> # A tibble: 4 × 51
#>   step_id timestamp           pair_id     i     j     A     B     Y status
#>     <int> <dttm>                <int> <int> <int> <int> <int> <int> <chr> 
#> 1       1 2026-02-10 23:43:44       1     1     4     1     4     0 ok    
#> 2       2 2026-02-10 23:43:44       2     4     8     4     8     0 ok    
#> 3       3 2026-02-10 23:43:44       3     8     2     8     2     1 ok    
#> 4       4 2026-02-10 23:43:44       4     2     6     2     6     0 ok    
#> # ℹ 42 more variables: round_id <int>, round_stage <chr>, pair_type <chr>,
#> #   used_in_round_i <int>, used_in_round_j <int>, is_anchor_i <lgl>,
#> #   is_anchor_j <lgl>, stratum_i <int>, stratum_j <int>, dist_stratum <int>,
#> #   stage_committed_so_far <int>, stage_quota <int>, is_explore_step <lgl>,
#> #   explore_mode <chr>, explore_reason <chr>, explore_rate_used <dbl>,
#> #   local_priority_mode <chr>, long_gate_pass <lgl>, long_gate_reason <chr>,
#> #   star_override_used <lgl>, star_override_reason <chr>, …

if (FALSE) { # \dontrun{
# Live run with OpenAI gpt-5.1 + flex priority.
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
} # }
```
