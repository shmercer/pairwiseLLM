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
  warm_start_mode = NULL,
  replay_reservoir = NULL,
  warm_start_trueskill = NULL,
  bootstrap_policy = "shuffled_connected"
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

  Integer seed used for deterministic connected-bootstrap choices and
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
  and `trueskill_pollitt` select direct pairs after the configured
  connected bootstrap and currently require `run_mode = "within_set"`.
  Unknown fields and invalid values abort with an actionable error. See
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
  for the full list of supported keys, detailed semantics, and defaults.

- checkpoint_every_steps:

  Optional positive integer checkpoint cadence for ordinary live
  persistence. If `NULL`, defaults to `100L`.

- warm_start_model:

  Optional calibrated model, cross-task or same-task algorithm ensemble,
  path string, or loader reference list (`name`/`source` or `path`).
  Mutually exclusive with `warm_start_prior`. Resolve and predict once
  when creating an assessment.

- warm_start_prior:

  Optional
  [`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md)
  object covering all items. Saved numeric scores are centered within
  each BTL refit scope.

- warm_start_features:

  Optional precomputed feature rows for model input; otherwise use item
  texts. Precomputed prediction needs neither Python nor a fitting
  backend.

- warm_start_python:

  Explicit Python interpreter for text extraction only.

- warm_start_prior_sd:

  Optional user-chosen raw theta prior SD for model input; scalar or
  per-item vector, default 0.5. Supplied prior objects retain their SDs.
  With `warm_start_trueskill = "predictive_distribution"`, explicit SD
  is required for model input, including `trueskill_only`, and also
  initializes TrueSkill sigma. Otherwise `trueskill_only` rejects this
  argument.

- warm_start_mode:

  Predictive destination: `cold` (neither model), `btl_only` (BTL
  prior), `trueskill_only` (TrueSkill locations), or `both` (both
  models). Omitted/NULL mode defaults to `btl_only` with predictive
  input, otherwise `cold`. Request `both` explicitly to initialize both
  models. In TrueSkill-warm modes, exact item-ID alignment precedes
  `mu = mu0 + sigma0 * prior_mean`, with `mu0 = 25`, `sigma0 = 25/3`,
  fixed multiplier 1, and unchanged sigma unless `warm_start_trueskill`
  explicitly requests distribution initialization. Explicit `cold` with
  predictive input, or a non-cold mode without it, errors.

- replay_reservoir:

  Optional
  [`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md)
  object. Requires ordinary within-set mode and a matching reservoir
  replay judge. Uses a seeded spanning-tree bootstrap and at most one
  committed observation per allowed unordered edge, always in its frozen
  observed orientation. On resume, omit this argument or supply the
  identical reservoir.

- warm_start_trueskill:

  Optional explicit uncertainty policy: NULL retains legacy
  initialization; `"predictive_distribution"` requires `trueskill_only`
  or `both` and maps `mu = 25 + (25/3) * prior_mean` and
  `sigma = (25/3) * prior_sd`, with `beta = 25/6`. It overrides
  constructor means and sigmas, aligns by exact item ID, and never clips
  SD. Model input requires explicit `warm_start_prior_sd`; prior objects
  use their stored SD. Named SD vectors align by ID; unnamed vectors
  follow prediction-input order. Omit this argument on resume; the saved
  distribution policy is authoritative.

- bootstrap_policy:

  Initial graph policy, default `"shuffled_connected"`.
  `"predictive_connected"` requires a selectable `replay_reservoir`,
  ordinary within-set mode,
  `warm_start_trueskill = "predictive_distribution"`, and
  `adaptive_config = list(pairing_strategy = "trueskill_pollitt")`.
  Build the reservoir from selectable primary observations only,
  excluding held-out edges and reversal audits. The graph uses only
  manifest endpoints, frozen initial TrueSkill means/SDs, and the seed,
  never outcomes. The queue is built once before judging and retained
  across updates and resume. On wrapper resume, omit this argument or
  supply the saved policy; a different policy or explicit predictive
  initialization seed is rejected.

## Value

An adaptive state object containing `step_log`, `round_log`, and
`item_log`. The object includes class `"adaptive_state"`, item ID
mappings, TrueSkill state, frozen bootstrap policy/queue, refit
metadata, and runtime configuration.

## Details

This function creates the stepwise controller state and seeds all
canonical logs used in the adaptive pairing workflow. The default
connected bootstrap uses a seeded shuffled chain, or a shuffled allowed
tree for replay reservoirs. Either graph policy connects all items after
\\N - 1\\ committed comparisons.

Pair selection in this framework is stepwise and uncertainty-aware.
Within-set/Phase-A hybrid routing uses TrueSkill ranks, strata, rolling
anchors, pair probabilities, and base utility \$\$U_0 = p\_{ij}(1 -
p\_{ij})\$\$ where \\p\_{ij}\\ is the current TrueSkill win probability
for pair \\\\i, j\\\\. Linking Phase A preparation requires an explicit
`adaptive_config$link_estimation_mode`. Adaptive Phase B selection
remains unavailable pending separate validation. Use
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
and
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
with explicit cross-set evidence and frozen shared judge parameters for
E1–E3 linking. The within-set/Phase-A hybrid long-link gate uses
TrueSkill throughout. Bayesian BTL supplies item estimates, posterior
uncertainty, EAP reliability, diagnostics, stopping, and the existing
`global_identified` signal. This signal can affect later hybrid tapering
and routing; selection is not wholly independent of BTL. Direct
within-set strategies use their documented partner targets after the
common bootstrap. Automatic Phase B selection is unavailable pending
separate validation. Linking Phase B refits use Bayesian posterior
estimation and posterior summaries/diagnostics are logged per spoke at
each linking refit.

The returned state contains canonical logs:

- `step_log`: one row per attempted step,

- `round_log`: one row per posterior refit,

- `item_log`: per-item posterior summaries by refit.

If `session_dir` is supplied, the initialized state is persisted
immediately using
[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md).

Predictive destinations and initial connectivity are separate choices.
By default, every warm mode retains the same seeded shuffled bootstrap
of N - 1 valid comparisons. The explicit predictive graph policy uses a
frozen allowed spanning tree with Pollitt probability targets and
degree-cap relaxation. Both policies preserve invalid-result retries and
recorded reservoir orientation. Later pairing retains the configured
strategy. When expressing budgets as mean comparison exposures per item,
each committed pair contributes two exposures. A prespecified
rounding-down convention gives `floor(B * N / 2)` total pairs, including
bootstrap, with realized exposure `2 * committed_pairs / N`. B = 0 is
prior-only; B = 0.5 and B = 1 precede connectivity and are not fully
connected comparative-judgment estimates. The tree completes at N - 1
successful commits; comparison N uses the post-bootstrap strategy.
Invalid attempts and retries do not add evidence. See
[`vignette("adaptive-warm-start")`](https://shmercer.github.io/pairwiseLLM/articles/adaptive-warm-start.md)
for the five-arm synthetic count example. Without the distribution
opt-in, BTL prior SD does not determine TrueSkill sigma. Ensemble
disagreement never supplies SD automatically. No historical
training-score units are restored.

Distribution initialization assumes the supplied BTL prior SD and
TrueSkill uncertainty describe comparable latent scales under the
documented affine convention. BTL priors apply to `theta_raw`; centering
induces dependence, so this is not the marginal SD of centered BTL
effects and does not equate the models' posteriors. Existing BTL
identifiability and prior rules are unchanged. Upstream predictive
workflows must establish uncertainty calibration using training data
only; supplying SD explicitly does not establish calibration. Scalar SD
supports sensitivity analyses, not essay-specific calibration. Saved
metadata records the versioned mapping, SD source and scalar/per-item
rule, and integrity digests; calibration is recorded as upstream,
unverified.

Predictive BTL priors apply only in `btl_only` and `both`, including
run-required linking Phase A. TrueSkill initialization applies in
`trueskill_only` and `both`. Imported Phase-A artifacts retain their own
generation identity and are not rerun because predictive input exists.
Linking and pooled judge refits keep their existing prior rules;
predictive evidence is not injected into Phase B priors, D-optimal
selection, or probes. Custom BTL fit functions should consume
`state$predictive_prior` only when `state$meta$warm_start_mode` is
`btl_only` or `both`; its presence alone does not imply BTL warming.
Resume preserves saved predictions, current TrueSkill state, mode,
strategy, and bootstrap progress; omit all warm-start arguments.

## Phase B linking restriction

Adaptive Phase B D-optimal selection is unavailable pending a separate
selector validation study. This includes all E1–E3 engines and legacy
D-optimal aliases; execution fails before selecting or judging a Phase B
pair. Phase A and ordinary within-set ranking remain available. For
linking, use
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
and
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
with explicit cross-set evidence and an explicit estimator choice.

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
[`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md),
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
# Prior-only exposure checkpoint: B=0 commits no comparisons.
B <- c(0, 0.5, 1, 2)
data.frame(B = B, target_pairs = floor(B * length(state$item_ids) / 2))
#>     B target_pairs
#> 1 0.0            0
#> 2 0.5            0
#> 3 1.0            1
#> 4 2.0            3
```
