# Issue 259: expanded warm-start architecture

Authority: [issue #259](https://github.com/shmercer/pairwiseLLM/issues/259), read in
full including its D041 governance comment, and the user's approved seven-phase
plan. This note records implementation decisions; it does not replace the issue.
Baseline: `013f869b1ba2272b88e85937023d57eb2c2c99fa`, package 1.5.1, R >=4.4.
Target: 1.5.2 in Phase 7. No study outcomes or production models informed this
design. Synthetic fixtures establish compatibility, not predictive validity.

## Inventory and boundaries

All 19 `R/warm_start_*.R` files were inspected. The map below identifies current
assumptions rather than proposing broad unrelated refactoring.

| Subsystem | Current sources (under R/) | Required seam |
|---|---|---|
| Schema/extraction | warm_start_feature_schema, warm_start_features, warm_start_python | Explicit v1/v2 resource dispatch; decoder currently filters by default v1 |
| Training | warm_start_cv, warm_start_tune | glmnet dependency, paths, alpha/lambda selection, refits, outer coefficient multiplication |
| Shared statistics | warm_start_preprocess, warm_start_calibration | Reuse split-local preprocessing, context outcome scaling, weighted losses and calibration |
| Model/audit | warm_start_model, warm_start_validation | Formats 1/2, engine==glmnet, coefficients, nonzero counts, glmnet trace validation |
| Prediction/introspection | warm_start_predict, warm_start_coefficients | Linear predictor and calibrated coefficients; engine-aware dispatch |
| Cross-task ensemble | warm_start_ensemble, warm_start_predictions | Existing class/artifact dispatch and prediction summary semantics |
| Persistence | warm_start_model_artifact, warm_start_model_io, warm_start_model_registry, warm_start_bundle | Reduction whitelist, format-2 bundle gate, artifact-type metadata |
| Consumers | warm_start_prior, warm_start_adaptive | Prior metadata currently rejects model format 3; adaptive path already consumes numeric predictions |

The `.R` suffix is omitted in the table. Source searches covered `engine`,
`format_version`, `coefficients`, `intercept`, `alpha`, `lambda`, `n_nonzero`,
`pairwiseLLM_warm_model`, `pairwiseLLM_warm_ensemble`, and their validators/helpers.
No provider or adaptive-selection redesign is necessary.

Inspected resources: the installed v1 CSV; all 116 inventory rows; feature-schema
audit, environment/value manifests, Python requirements locks, setup/audit
scripts; `inst/python/pairwisellm_warm_start.py` and its README; the maintainer
model builder/README; warm-start Roxygen/Rd, README overview and vignette.

Inspected tests: 0031, 0100-0112, 3100-3102, 5034, 5100/5101/5105, 6100,
9100-9102, warm core/bundle helpers and Python golden/backend fixtures. Adjacent
3103-3106 and 5102-5108 protect persistence, replay and reservoir consumers.
The test README governs ordering and mocks. Tests 0100 and 0104 intentionally
reject v2 and format 3 today; move those unsupported-value examples only in the
phase adding support. Tests asserting legacy format/reduction behavior must
continue to use genuine legacy fixtures, not weaken the old validators.

## Public surface and engine-neutral model contract

Append to the existing public fitting signature, preserving positional arguments:

```r
engine = c("glmnet", "pls", "svr_rbf")
cv_plan = NULL
engine_control = NULL
```

Default engine remains glmnet; schema remains writing_features_v1. Existing
alpha_grid/lambda_rule semantics are unchanged for glmnet. Explicitly supplied
glmnet-only controls on another engine fail rather than being silently ignored.
Omitted legacy defaults are harmless for new engines. `engine_control` accepts
only `ncomp` for PLS or `cost`/`gamma_multiplier` for SVR. Epsilon stays 0.10;
method, rank tolerance, loss, SE and tie rules are fixed. Unknown controls fail.
For glmnet, controls continue through the existing public arguments; nonempty
engine_control fails. No automatic engine/component selection is added.

Retain `pairwiseLLM_warm_model`. Dispatch model format validation before reading
engine-specific fields:

- Format 1 remains the original fully audited glmnet model (or uncalibrated
  internal fixed fit). Format 2 remains the original summary-only glmnet model.
  Preserve their constructors, validators, interpretation and loading behavior.
- Newly fitted public models use format 3, with `audit_status = "full"` or
  `"summary_only"`. Keep common schema/features, preprocessing, outcome,
  calibration, training, tuning, validation and optional metadata fields.
- Format 3 adds the common CV-plan identity/evidence and a typed
  `engine_payload`. `training` records engine/version, package version, task,
  sample size and selected hyperparameters. Engine dispatch validates complete
  traces and every outer record, including rederived selections/calibration/
  metrics; it is not permission to accept less audit evidence.
- For linear engines retain top-level `coefficients`/`intercept` and relevant
  existing glmnet training fields as compatibility views, checked against the
  payload. For nonlinear SVR, these linear-only fields are NULL and never used
  to imply feature coefficients. Summaries report engine-specific parameters.

The common orchestration owns outer/inner contexts, preprocessing, tuning loss,
OOF calibration, validation and final deployment order. Engine adapters own
candidate construction, fitting, numeric payload extraction and raw prediction.
The existing glmnet reference path and its exact penalty fitting stay intact.
No raw training text or opaque backend fit is stored.

### Portable payloads

- `type = "linear"`: retained-feature-ordered named coefficients and intercept.
  For PLS use equivalent beta and `Ymeans - Xmeans %*% beta` from the selected fit.
- `type = "rbf_svr"`: retained-feature-ordered numeric support-vector matrix,
  dual coefficient vector, rho and actual positive gamma. Prediction is
  `exp(-gamma * squared_distance) %*% dual - rho`. Validate all dimensions,
  feature names and finite values. Clamp only roundoff-negative squared
  distances to zero. No backend package is needed for deployment.

`warm_start_coefficients()` retains its current meaning for glmnet and adds PLS
linear coefficients on the same stored standardized predictor scale. SVR and an
ensemble containing SVR produce an informative typed error naming the nonlinear
component. No fabricated nonlinear feature weights are reported.

## Shared CV plan and statistical invariants

New public constructor:

```r
make_warm_start_cv_plan(ids, theta, task_id,
  seed = 1L, outer_folds = 5L, inner_folds = 5L)
```

Use an S3 list `pairwiseLLM_warm_cv_plan`, format 1, containing task_id, ordered
normalized item IDs, exact numeric theta, within_task_z/sample-SD conventions,
seed/RNG-kind provenance, outer fold vector, each ordered outer-training subset's
inner fold vector, full-data inner fold vector, and integrity digest. Names/IDs
bind vectors explicitly. Do not sort input IDs or tie outcomes differently.
The plan is schema-independent so the same evaluation partitions can be reused
across representations; ensemble construction separately enforces schema identity.

Use the existing version-2 XDR hashing convention that omits writer-version words
for a stable integrity digest. Canonical fields have fixed order and no timestamps,
paths or engine metadata. This digest detects accidental changes, not authorship.
`saveRDS`/`readRDS` provide portable persistence; no separate I/O API is necessary.

Generate assignments once within locally preserved RNG state, using the existing
outcome-ranked blocks and randomized ties/labels in this exact draw order:

1. Full-data outer folds.
2. Inner folds on each outer training subset, outer fold 1 through K.
3. Full-data inner folds.

Current glmnet fitting does not consume RNG. Generating this sequence before any
engine fit preserves legacy folds while insulating them from backend RNG use.
Preserve caller RNGkind, including nondefault kinds, and present/absent
`.Random.seed`. Record actual kinds. A supplied plan is never regenerated.

Validate plan integrity, task, ordered IDs, theta, fold coverage/counts and legal
training outcomes before extraction or fitting. Do not silently align a different
ordered ID/outcome vector. Feature tables retain existing alignment to requested
IDs. Omitted seed/fold arguments defer to the plan; explicit conflicts fail using
`missing()` to distinguish omission. Never shrink requested fold counts.

Frozen fitting invariants:

- Learn predictor missingness filtering, imputation, NZV removal and scaling only
  from each training split, preserving all current thresholds and conventions.
- Each outer/full context learns its outcome mean/sample SD. Its inner training
  and inner holdout outcomes use that context scale. Outer holdout observations
  use their outer training scale, not the global full-data outcome scale.
- Preserve glmnet's reference paths computed on each tuning context, exact inner
  penalties, 41-alpha default, solver tolerance/iterations, alpha/lambda ties and
  lambda.1se/lambda.min behavior. Do not reinterpret this reference-path policy.
- Reuse observation-count-weighted inner MSE and
  `sqrt(weighted.mean((fold_mse-cvm)^2, sizes)/(K-1))` with tolerance
  `1e-10 * max(1, abs(a), abs(b))` for numerical ties.
- Learn OOF linear calibration from training-side selected-hyperparameter OOF
  predictions; QR tolerance stays 1e-7, finite negative slopes remain legal.
  Degenerate calibration is an explicit error, never an identity fallback.
- Only untouched outer predictions define validation. Fit final deployment
  parameters after outer validation, full-data tuning and OOF calibration.

PLS uses optional `pls`, explicit `kernelpls`, no backend scaling/CV, and a common
grid within each tuning context bounded across every required inner training
matrix and context refit by rank, p, n-1 and 10 (rank tolerance 1e-7). Explicit
ncomp candidates must all be legal. 1-SE selects fewer components. Rank alone
does not prevent zero-covariance/latent saturation failures: detect nonfinite
fits and fail contextually, without candidate omission or algorithm fallback.

RBF-SVR uses optional `e1071`, epsilon-regression/radial, `scale = FALSE`,
`cross = 0`, no probability fitting; default cost `2^(-2:4)`, gamma multiplier
`2^(-2:2)`, epsilon 0.10. Each fit sets gamma to multiplier/its retained p.
Weighted MSE/SE follows the shared contract. Minimum-error ties and eligible
1-SE choices favor lower cost, then lower gamma multiplier. Store complete traces.

### Phase 2 implemented representation

Public fits now emit format 3; internal fixed fits retain legacy format 1.
`cv_plan` contains the complete `pairwiseLLM_warm_cv_plan` object in full models.
`cv_identity` stores its format/digest, task/n, exact ordered ID/outcome digest,
seed, fold counts and RNG kinds. Reduction retains this identity and sets
`cv_plan = NULL`; it preserves format 3 with `audit_status = "summary_only"`.
Full validation cross-checks every stored partition and transformation with the
plan, in addition to the original complete glmnet statistical audit.

`engine_payload = list(type = "linear", coefficients = ..., intercept = ...)`
is the current deployment boundary. `training$hyperparameters` records selected
alpha/lambda; legacy top-level views and training fields remain checked.
PLS/SVR names fail explicitly until their assigned phases. Engine-specific
numeric payload validation/copy/prediction is isolated from CV orchestration.
Future payloads must extend that typed boundary, not weaken the legacy validator.

New-format prediction metadata records engine/version and CV digest. Registry
rows add engine/version and named component-engine metadata. Existing cross-task
rows still have n=NA and component-only validation. Legacy bundle component
records retain their original JSON structure; format-3 records additionally
carry compact CV identity. Neither schema extraction nor publication mechanics
changed. Test prefixes 0114, 3107 and 9104 are used by this phase.

## V2 feature contract

### Phase 4 PLS implementation

PLS is now an optional development engine (`pls` in Suggests); the public default
remains glmnet. Only `engine_control$ncomp` is accepted for PLS. NULL/empty controls
use the default grid; explicit candidates are sorted unique positive integers
no greater than ten. Explicit glmnet-only arguments fail on PLS.

`warm_start_pls.R` fits explicit `kernelpls` with `scale=FALSE`,
`validation="none"`, and centering. Every context stores a common grid bounded
by its context-refit and all inner-training matrices, using centered non-LAPACK
QR with tolerance 1e-7, p, n-1 and ten. Failed candidates are never omitted.
Weighted MSE/SE and numerical ties retain the locked formulas, and the 1-SE
selection favors fewer components. Shared orchestration still owns context
outcome scaling, split-local preprocessing, calibration and outer validation.

Full PLS tuning records `ncomp_requested`, `ncomp_grid`, per-fit `bounds`, all
candidate OOF predictions and fold losses in `traces`, and selected OOF evidence.
The PLS audit validator reconstructs losses, weighted summaries, choices and
calibration; shared validation checks every outer record against the CV plan.
Stored ranks are checked against n/p and grid bounds, but independently
recomputing ranks requires the original feature table, which is not stored.
This is an integrity/audit contract, not an authenticity guarantee.

Format 3 records `training$hyperparameters$ncomp` and no invented alpha/lambda.
The payload remains `type="linear"` with retained-feature-ordered coefficients
and `Ymeans - Xmeans %*% beta`. Full/reduced validation, prediction and coefficient
inspection require no PLS namespace. Reduction keeps the final grid/settings,
compact CV identity, deployment parameters and labeled validation summaries;
it drops rank records, candidate/selected OOF rows and outer fold evidence.
The legacy glmnet projection remains unchanged; PLS never impersonates a legacy
glmnet model to bypass its validation. RBF-SVR remains reserved for Phase 5.

V1 CSV SHA-256 remains
`1414573759c302dc24e9041cfe2eb084fb4be1fac1fd26440b2011d4f9a736f7`.
Preserve v1 CSV, inventory, audit evidence, locks and golden fixtures. No new
Python dependency is needed. Existing audited versions include spaCy 3.7.5,
TextDescriptives 2.8.4, textstat 0.7.13 and en_core_web_lg 3.7.1.

The user approved this source-only representative policy: retain v1 first, then
eligible primitive scalar counts/summaries; among algebraically equivalent
formulas prefer the widest-defined representative, then original inventory
order. Order the chosen additions by original inventory order. Apply algebraic
exclusions against the final retained set, not only the old v1 labels.

The source-audited v2 membership is the original 20 features plus:

```text
n_characters
token_length_median
sentence_length_median
syllables_per_token_mean
syllables_per_token_median
syllables_per_token_std
pos_prop_aux
pos_prop_det
pos_prop_part
prop_adjacent_dependency_relation_std
second_order_coherence
gunning_fog
lix
textstat_char_count
textstat_letter_count
textstat_lexicon_count
textstat_miniword_count
textstat_syllable_count
textstat_sentence_count
textstat_polysyllabcount
textstat_linsear_write_formula
textstat_difficult_words
textstat_gunning_fog
textstat_spache_readability
textstat_long_word_count
textstat_monosyllabcount
```

Phase 3 materialized the explicit 46-row CSV and full 116-candidate audited
inventory in schema-only commit `8e61988`, with source-wording corrections in
metadata-only commit `3509f26`, before consumer validation/commit. Final CSV
SHA-256: `d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492`.
The installed `python/schema-writing-v2.json` records this hash and inventory
identity. See `feature-schema-v2-audit.md` and `feature-inventory-v2.csv` for
precise definitions, source defaults and conditional exclusions. Membership and
order match the approved list above; original v1 evidence remains immutable.

Retain TD and textstat character counts: literal-space versus regex-whitespace
removal differ. Keep textstat difficult_words (unique=True, threshold=2) distinct
from readability occurrence counts. Spache precedes conditionally equivalent
Dale-Chall v2; TD Fog has a wider defined domain than SMOG; TD LIX precedes RIX.
Linsear-Write uses its first-100-word default. Retained primitive counts make
several other readability formulas algebraic restatements. Cross-package names
alone do not establish equivalence. Preserve pinned defaults and missing values.

Implement schema-aware R decoding/Python request hashing, explicit textstat
method dispatch (currently every row calls Dale-Chall), v2 missingness, and the
TD readability component. Keep the v1 extraction path semantically unchanged.
Do not load study data to select fields, grids or exceptions.

## Same-task ensemble and downstream interoperability

Add `ensemble_warm_start_algorithms(...)`, accepting at least two named model
components. Preserve names/order; reject nested ensembles. Use standalone S3
class `pairwiseLLM_warm_algorithm_ensemble`, its own format 1 and
`artifact_type = "algorithm_ensemble"`; never subclass the old strict ensemble.

Require fully audited format-3 components with equal task, ordered item IDs,
exact outcome identity/standardization, schema and CV-plan identity, OOF-linear
calibration and complete aligned outer validation. Verify observed values and
folds as well as digests. Legacy models need refitting for verifiable plan
identity; reduced models lack evidence to construct a new honest ensemble.

Deployment is the equal arithmetic mean of component calibrated predictions.
Validation is the equal arithmetic mean of aligned outer-held-out calibrated
predictions evaluated against the matching per-outer-context observed values.
No ensemble-level calibration or learned weights. Component SD is diagnostic,
never a Bayesian prior SD. Keep all component audits in a full ensemble.

Reduction of an already constructed ensemble retains component deployment
payloads, common identity digests and labeled summary-only validation metrics;
it does not invent reconstructible row evidence. A typed whitelist copier must
preserve SVR matrix shape/dimnames while stripping unknown fields/attributes.
The existing `.warm_start_plain()` strips matrix dimensions and cannot be reused
unchanged. Support vectors are necessary numeric deployment data; reduction is
not a claim of anonymization.

Prediction, print/summary, save/load, prepare/reduce, registry, bundle records and
prior metadata must dispatch on the new class/format. Add engine/version and
component-algorithm registry metadata without changing old rows' meanings.
Old cross-task rows retain artifact_type='ensemble', n=NA, and component metrics;
new same-task rows expose common n and honest ensemble validation.

**Phase 2 dependency:** format-3 single-model storage/reduction/registry/bundle/
prior interoperability must land with new public glmnet fits. Deferring these
until Phase 6 would break default workflows. Phase 6 adds the new ensemble class.
Storage publication mechanics remain unchanged; existing adaptive code already
uses numeric prediction/prior values. Preserve prior centering, prior SD, BTL/
TrueSkill modes, persistence and resume authority. No Phase B changes.

Do not repurpose `ensemble_warm_start_models()`. It retains equal-weight
cross-task semantics and component-only validation, and continues accepting
same task labels/repeated models as current tests require. Do not reconfigure
the existing v1/glmnet maintainer model builder or fit production study models.

## Verification, phase delivery and handoff

See [the execution ledger](issue-259-handoff.md) for branch/PR/task status,
commands/results, limitations and new-thread prompts. Every phase ends with
focused tests, reviewed diffs, commits/PR and a durable handoff. Use a new thread
for the next phase; no reliance on conversational memory or /tmp logs.

Phase 1 freezes pre-refactor fixtures. Phase 2 proves exact fold/discrete-choice
and numerical compatibility. Phase 3 proves exact schema/order/hash and synthetic
extraction/missingness. Phases 4/5 prove leakage boundaries, weighted tuning/ties,
legal grids, reference-backend predictions, artifact corruption and engine-free
deployment. Phase 6 independently reconstructs outer ensemble validation and
exercises storage/prior/adaptive paths. Phase 7 completes docs and package quality.

Full tests, current coverage review (target >=95% for new/materially affected R
files), CRAN-style check, documentation regeneration/renders, lint and CI are
explicitly authorized for Phase 7. Report exact results, skips, attributable
notes and unresolved coverage gaps. No new errors/warnings. Optional dependencies
remain Suggests with explicit guards; retain R >=4.4 and no install side effects.

Final delivery records changed files by phase, APIs, artifact compatibility,
exact schema/hash, dependencies, tests/checks/coverage, docs, limitations and the
exact reviewed candidate SHA. A later merge SHA is a different D042 candidate
and must be identified and validated before downstream pinning.
