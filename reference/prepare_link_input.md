# Prepare explicit evidence for a linking estimator

These development interfaces define the common contract for E1–E3. They
do not select pairs, contact providers, or depend on adaptive state. E1
is implemented with deterministic quadrature; E2 and E3 report an
unavailable estimator instead of running a legacy linker.

## Usage

``` r
prepare_link_input(
  estimator,
  hub,
  spoke,
  phase_a,
  cross,
  judge,
  control = list(),
  provenance = list()
)
```

## Arguments

- estimator:

  Required exact ID: `fixed_shape_offset`, `gaussian_posterior_bridge`,
  or `joint_offset`. There is no default.

- hub, spoke:

  Lists with scalar `set_id` and an `items` data frame containing unique
  `item_id` and optional `global_item_id`. Local IDs may overlap between
  sets; supplied global IDs must be unique across both sets. Items are
  ordered hub first, then spoke, and by bytewise item ID within each
  set.

- phase_a:

  Named list with `hub` and `spoke` entries. Each entry contains exactly
  one statistical payload: E1 `points` (named numeric vector), E2
  `draws` (draws by named item columns), or E3 `observations` (table as
  below). Points and each draw are separately centered, with removed
  means recorded. Optional `source` metadata contains `artifact_hash`,
  `evidence_hash`, and `n_observations`; unavailable values remain typed
  missing. External source hashes are assertions of provenance, distinct
  from computed payload hashes. E1 also accepts
  `list(artifact = artifact)` in either set entry, mutually exclusive
  with `points`. Supply an in-memory canonical Phase A artifact (use
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html) explicitly for
  files). Its `set_id`, `fit_model_id`, `n_items`, `n_pairs_committed`,
  and `items` are checked. Item-aligned `items$theta_raw_mean` values
  are the EAP source; global IDs must match when supplied. Phase B
  summaries are rejected. The original artifact is hashed, and its
  declared within-set evidence hash/count are retained as provenance;
  raw outcomes, posterior draws, and marginal SDs are not used for E1
  inference or retained in normalized input. Artifact `source` fields,
  if supplied, must agree with the extracted metadata. This extracts
  statistical inputs; it does not run adaptive Phase A
  quality/reliability gates.

- cross:

  Explicit active cross-set observations, including an empty table at
  zero budget. Evidence tables contain `observation_id`, `A_set`,
  `A_item`, `B_set`, `B_item`, and numeric binary `y_A` (one means A
  won). IDs identify judgments, not pairs, and must be unique across all
  raw evidence supplied. Repeated judgments of a pair use different IDs.
  All rows are used in supplied order; invalid rows are rejected rather
  than dropped. Phase A observations use the same columns and must be
  within the corresponding set.

- judge:

  Named list with finite `beta`, `epsilon` in `[0,1]`, `model_variant`
  (`btl`, `btl_b`, `btl_e`, or `btl_e_b`), `link = "logit"`, and scalar
  `source`. Omitted model components must have value zero.

- control:

  List with `delta_prior = list(mean = 0, sd = 5)`, numerical
  `estimator` controls, and optional named `initial` free-coordinate
  vector. E1 accepts positive `rel_tol = 1e-9`, `abs_tol = 1e-11`,
  `quantile_tol = 1e-8`, and integer `subdivisions = 1000L`. The
  subdivision limit bounds the number of quadrature panels and CDF
  integration/root iterations. Tolerances apply to normalized mass and
  moments in prior-SD coordinates; `quantile_tol` is in delta units.
  Effective defaults are logged with every fit. Other estimators
  currently accept no numerical controls. Initial values are
  optimization hints only; E1 does not use them.

- provenance:

  List with optional `source_commit` (package source revision; unknown
  is `NA_character_`) and `expected`, a named subset of the computed
  `hashes` and `counts` lists against which to reconcile inputs. Hashes
  use a versioned
  [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) scheme;
  the producing rlang version is recorded.

## Value

A versioned `pairwiseLLM_link_input` list with normalized identities,
evidence, coordinates, controls, hashes, counts, and provenance. Raw
Phase A likelihood counts are zero for E1/E2; source observation counts
are separate.

## Examples

``` r
hub <- list(set_id = "H", items = data.frame(item_id = c("h1", "h2")))
spoke <- list(set_id = "S", items = data.frame(item_id = c("s1", "s2")))
cross <- data.frame(observation_id = character(), A_set = character(),
  A_item = character(), B_set = character(), B_item = character(), y_A = integer())
input <- prepare_link_input("fixed_shape_offset", hub, spoke,
  phase_a = list(hub = list(points = c(h1 = -1, h2 = 1)),
    spoke = list(points = c(s1 = -.5, s2 = .5))), cross = cross,
  judge = list(beta = 0, epsilon = 0, model_variant = "btl",
    link = "logit", source = "frozen Phase A"))
input$counts
#> $phase_a_hub
#> [1] 0
#> 
#> $phase_a_spoke
#> [1] 0
#> 
#> $cross
#> [1] 0
#> 
#> $source_hub
#> [1] NA
#> 
#> $source_spoke
#> [1] NA
#> 
fit <- fit_link(input)
fit$offset
#> $delta_mean
#> [1] 0
#> 
#> $delta_sd
#> [1] 5
#> 
#> $delta_lower
#> [1] -9.79982
#> 
#> $delta_upper
#> [1] 9.79982
#> 
#> $identification
#> [1] "prior_only"
#> 
predict_link(fit, data.frame(observation_id = "held-out-1",
  A_set = "H", A_item = "h1", B_set = "S", B_item = "s2"))
#> [1] 0.3887695
```
