# Adaptive ranking

Initialize an adaptive ranking session and canonical state object.

## Usage

``` r
adaptive_rank_start(
  items,
  seed = 1L,
  session_dir = NULL,
  persist_item_log = FALSE,
  ...,
  adaptive_config = NULL,
  checkpoint_every_steps = NULL,
  warm_start_model = NULL,
  warm_start_prior = NULL,
  warm_start_features = NULL,
  warm_start_python = NULL,
  warm_start_prior_sd = NULL,
  warm_start_mode = NULL
)
```

## Arguments

- items:

  A vector or data frame of items. Data frames must include an `item_id`
  column (or `id`/`ID`). For linking run modes, items must also include
  integer `set_id` values and globally unique `global_item_id` values.
  Item IDs may be character; internal logs use integer indices derived
  from these IDs.

- seed:

  Integer seed used for deterministic connected-bootstrap shuffling and
  selection randomness. Default is `1L`.

- session_dir:

  Optional directory for saving session artifacts. Default is `NULL`.

- persist_item_log:

  Logical; when TRUE, write per-refit item logs to disk. Default is
  `FALSE`.

- ...:

  Internal/testing only. Supply `now_fn` to override the clock used for
  timestamps.

- adaptive_config:

  Optional named list of adaptive controller overrides.
  `pairing_strategy` defaults to `hybrid`; `random`, `trueskill_p50`,
  and `trueskill_pollitt` select direct pairs after the common connected
  shuffled bootstrap and currently require `run_mode = "within_set"`.
  Unknown fields and invalid values abort with an actionable error. See
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
  for the full list of supported keys, detailed semantics, and defaults.

- checkpoint_every_steps:

  Optional positive integer checkpoint cadence for ordinary live
  persistence. If `NULL`, defaults to `100L`.

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
  vector, default 0.5. Supplied prior objects retain their SDs. Not
  accepted with `trueskill_only`; never controls TrueSkill sigma.

- warm_start_mode:

  Predictive destination: `cold` (neither model), `btl_only` (BTL
  prior), `trueskill_only` (TrueSkill locations), or `both` (both
  models). Omitted/NULL mode defaults to `btl_only` with predictive
  input, otherwise `cold`. Request `both` explicitly to initialize both
  models. In TrueSkill-warm modes, exact item-ID alignment precedes
  `mu = mu0 + sigma0 * prior_mean`, with `mu0 = 25`, `sigma0 = 25/3`,
  fixed multiplier 1, and unchanged sigma. Explicit `cold` with
  predictive input, or a non-cold mode without it, errors.

## Value

An adaptive state object containing `step_log`, `round_log`, and
`item_log`. The object includes class `"adaptive_state"`, item ID
mappings, TrueSkill state, connected bootstrap queue, refit metadata,
and runtime configuration.

## Details

This function creates the stepwise controller state and seeds all
canonical logs used in the adaptive pairing workflow. Connected
bootstrap pair construction follows the same seeded shuffled chain in
every mode, giving a connected comparison graph after \\N - 1\\
committed comparisons.

Pair selection in this framework is stepwise and uncertainty-aware.
Within-set/Phase-A hybrid routing uses TrueSkill ranks, strata, rolling
anchors, pair probabilities, and base utility \$\$U_0 = p\_{ij}(1 -
p\_{ij})\$\$ where \\p\_{ij}\\ is the current TrueSkill win probability
for pair \\\\i, j\\\\. In linking Phase B, anchor/strata routing uses a
linking-global score derived from Phase A raw summaries and the accepted
Phase B linking state. In linking Phase B, eligible cross-set candidates
are ranked by ridge-stabilized D-optimal log-det information gain on the
active linking parameter block using order-averaged Model D
probabilities. In the spoke free block with the hub fixed. Linking
inference parameters are used for inference/diagnostics/stopping, not as
direct selection objectives. Phase B uses pooled within-set Phase A
judge-parameter estimates, using the configured BTL model variant, as
the accepted shared source for fixed `beta`/`epsilon` constants. The
within-set/Phase-A hybrid long-link gate uses TrueSkill throughout.
Bayesian BTL supplies item estimates, posterior uncertainty, EAP
reliability, diagnostics, stopping, and the existing `global_identified`
signal. This signal can affect later hybrid tapering and routing;
selection is not wholly independent of BTL. Direct within-set strategies
use their documented partner targets after the common bootstrap. Phase B
selection and prior rules are unchanged. Linking Phase B refits use
Bayesian posterior estimation and posterior summaries/diagnostics are
logged per spoke at each linking refit.

The returned state contains canonical logs:

- `step_log`: one row per attempted step,

- `round_log`: one row per posterior refit,

- `item_log`: per-item posterior summaries by refit.

If `session_dir` is supplied, the initialized state is persisted
immediately using
[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md).

Predictive initialization is separate from observed connectivity: every
mode retains the same seeded connected shuffled bootstrap of N - 1 valid
comparisons, with common presentation balancing and invalid-result
retries. Predictive locations can affect later TrueSkill-based
selection; they do not replace the initial observed spanning path. BTL
prior SD and ensemble diagnostics never determine TrueSkill sigma. No
historical training-score units are restored.

Predictive BTL priors apply only in `btl_only` and `both`, including
run-required linking Phase A. TrueSkill initialization applies in
`trueskill_only` and `both`. Imported Phase-A artifacts retain their own
generation identity and are not rerun because predictive input exists.
Transform, anchored-joint, and pooled judge refits keep their existing
prior rules; predictive evidence is not injected into Phase B priors,
D-optimal selection, or probes. Custom BTL fit functions should consume
`state$predictive_prior` only when `state$meta$warm_start_mode` is
`btl_only` or `both`; its presence alone does not imply BTL warming.
Resume preserves saved predictions, current TrueSkill state, mode,
strategy, and bootstrap progress; omit all warm-start arguments.

## See also

[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md),
[`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 11)
summarize_adaptive(state)
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       3               0               0        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
```
