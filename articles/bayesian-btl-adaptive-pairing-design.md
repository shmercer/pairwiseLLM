# Bayesian BTL Adaptive Pairing Design

## Bayesian BTL Adaptive Pairing Design

**Stable Rankings, Robust Inference, and Fully Auditable Adaptive
Sampling**

------------------------------------------------------------------------

### Scope and Goals

This document specifies a hybrid adaptive paired-comparison design for
ranking **N** items using:

1.  Online step-by-step pair selection driven by TrueSkill (fast,
    uncertainty-aware), and  
2.  Intermittent global inference using Bayesian Bradley–Terry–Luce
    (BTL) MCMC (Stan) for θ estimates, diagnostics, and stopping.

Selection is organized into Pollitt-style rounds that explicitly balance
global scale identification and local refinement. Each round schedules a
fixed number of committed comparisons and allocates them across four
pair types:

- Rolling anchor links, which connect non-anchor items to a small,
  dynamically updated set of reference items to provide high-bandwidth
  global calibration;
- Long-range links, which connect non-anchor items that are widely
  separated in the current ranking to strengthen global connectivity;
- Mid-range links, which connect moderately separated items to stabilize
  scale structure; and
- Local refinement links, which compare nearby items to improve
  fine-grained ordering.

Pair selection operates step-by-step within this round structure. At
each step, candidate pairs are generated according to the active round
stage and passed through a canonical selection pipeline that enforces
hard invariants, duplicate controls, star caps, and
exploration–exploitation routing. Pair utility is evaluated using a fast
online selection heuristic, while Bayesian Bradley–Terry–Luce inference
is used exclusively for estimating latent scores (θ), diagnostics, and
stopping decisions.

The design supports: \* 30 ≤ N ≤ 2000 items, with mechanisms that scale
to large N through percentile-based strata and bounded candidate sets;
\* a single judge producing binary outcomes (no ties); and \* fully
auditable logs at the step, round, and refit levels.

By explicitly budgeting comparisons that promote global linking while
retaining locally informative comparisons, the design aims to achieve
rapid global identifiability of the latent scale together with efficient
local discrimination.

------------------------------------------------------------------------

### User-facing overview

For a practical tutorial using
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
see:

- [Adaptive Pairing &
  Ranking](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md)

------------------------------------------------------------------------

### Global Definitions (normative)

#### Indices

- **Step**: one attempted selection and (if valid) one committed
  comparison.
- **Pair (committed comparison)**: a step that produced a valid judge
  outcome and was committed.

#### Counts and keys

- `pair_obs_count[{i,j}]`: number of **committed** comparisons observed
  for unordered endpoints `{i,j}`.
- `pair_obs_count_ordered[(A,B)]`: number of committed comparisons
  observed with ordered presentation `(A,B)`. Keyed by ordered endpoint
  ids (e.g., (i,j)), not by rank positions.
- `pair_key(i,j)`: canonical unordered key with `i<j` (implementation
  note: stable string or integer hash).

#### Ordering precedence

When constraints conflict, apply this priority order:

1.  **Hard invariants** (self-pairs forbidden; transactional commit;
    order-reversal on repeats)
2.  **Duplicate policy**
3.  **Star caps**
4.  **Exploration / quota routing**
5.  **Position-balance preference** (best-effort; never a hard filter)

This prevents deadlocks and ensures deterministic behavior across
implementations.

#### Terminology note (normative)

This design distinguishes between:

- **Rounds** (`round_id`): Pollitt-style scheduling units that allocate
  a fixed number of committed comparisons across pair types.
- **Refits** (`refit_id`): Bayesian BTL posterior updates triggered by
  accumulated committed comparisons.

Rounds occur more frequently than refits. Multiple rounds typically
occur between successive refits.

------------------------------------------------------------------------

### 1. Statistical Model

This design supports four closely related Bradley–Terry–Luce (BTL) model
variants. All variants share the same latent ability parameters
$\theta_{i}$ (one per item), and differ only in whether they include:

- a **global lapse / random‑response rate** $\epsilon$ (“sometimes the
  judge answers at random”), and/or
- a **global position bias** $b$ (“the judge slightly prefers the item
  shown in position A / first”).

**Default (recommended):** **BT + position bias + lapse** (Model D).  
Use simpler variants only when you have a reason (e.g., you are certain
there is no position effect, or you want the minimal model).

------------------------------------------------------------------------

#### 1.1 Notation (common to all variants)

For each comparison $k = 1,\ldots,M$:

- Items are presented in **ordered** positions: $A\lbrack k\rbrack$
  (position A / first) vs $B\lbrack k\rbrack$ (position B / second).
- Outcome:
  - `Y[k] = 1` means A wins (the judge chose the first item)
  - `Y[k] = 0` means B wins (the judge chose the second item)

Define the latent quality difference:

$$d_{k} = \theta_{A{\lbrack k\rbrack}} - \theta_{B{\lbrack k\rbrack}}$$

Define the baseline Bradley–Terry win probability:

$$\pi_{k} = \text{logit}^{- 1}\left( d_{k} \right)$$

------------------------------------------------------------------------

#### 1.2 Model options (choose one)

##### Model A — Regular BTL (no lapse, no position bias)

$$p_{k} = \text{logit}^{- 1}\left( \theta_{A{\lbrack k\rbrack}} - \theta_{B{\lbrack k\rbrack}} \right)$$

$$Y_{k} \sim \text{Bernoulli}\left( p_{k} \right)$$

**Interpretation:** the judge’s choice depends only on the latent
quality difference; all randomness is “ordinary” Bernoulli variation.

------------------------------------------------------------------------

##### Model B — BTL + lapse ($\epsilon$ )

\$\$ p_k = (1-),^{-1}(*{A\[k\]} -* {B\[k\]})

- \$\$

$$Y_{k} \sim \text{Bernoulli}\left( p_{k} \right)$$

**Interpretation:** most of the time the judge follows Bradley–Terry;
with probability ($\epsilon$) the judge responds like a fair coin‑flip.
This single parameter absorbs occasional random mistakes without
complicating the model.

------------------------------------------------------------------------

##### Model C — BTL + position bias ($b$)

$$p_{k} = \text{logit}^{- 1}\left( \theta_{A{\lbrack k\rbrack}} - \theta_{B{\lbrack k\rbrack}} + b \right)$$

$$Y_{k} \sim \text{Bernoulli}\left( p_{k} \right)$$

**Interpretation:** the judge has a consistent side preference:

- $b > 0$: favors position A (first)
- $b < 0$: favors position B (second)
- $b = 0$: no position preference

------------------------------------------------------------------------

##### Model D — BTL + position bias + lapse ($b,\epsilon$) (**default**)

\$\$ p_k = (1-),^{-1}(*{A\[k\]} -* {B\[k\]} + b)

- \$\$

$$Y_{k} \sim \text{Bernoulli}\left( p_{k} \right)$$

**Interpretation:** combines both protections:

- $b$ accounts for systematic position preference (if present)
- $\epsilon$ accounts for occasional random responding

This is the most robust default when the “judge” is an LLM or other
noisy rater.

------------------------------------------------------------------------

#### 1.3 Priors and identifiability (all variants)

- Raw abilities:

  $$\theta_{i}^{raw} \sim \mathcal{N}(0,1)$$

- Centered abilities (hard identifiability constraint):

  $$\theta_{i} = \theta_{i}^{raw} - \frac{1}{N}\sum\limits_{j = 1}^{N}\theta_{j}^{raw}$$

  Ensuring:

  $$\sum\limits_{i = 1}^{N}\theta_{i} = 0$$

- Lapse parameter (Models B and D):

  $$\epsilon \sim \text{Beta}(2,20)$$

- Position bias parameter (Models C and D):

  $$b \sim \mathcal{N}(0,0.3)$$

- **Interpretation:**

  - The normal prior on $\theta$ fixes the latent scale; centering
    removes additive indeterminacy.
  - The Beta prior reflects that random responses are expected to be
    rare but non‑zero.  
  - The Normal prior on $b$ is weakly informative: it allows mild
    position bias while shrinking extreme bias toward zero.

### 2. Data Structures and Invariants

#### 2.1 Core Stores

For each comparison $k = 1,\ldots,M$:

- `A[k]`, `B[k]`: item indices (ordered presentation)
- `Y[k]`: outcome (1 = A wins, 0 = B wins)

Tracking structures (all counts are over committed comparisons only):

- `deg[i]`: total committed comparisons involving item `i`
- `pos_count_A[i]`: number of times item `i` appears in position A
  (first) in committed comparisons
- `pos_count_B[i]`: number of times item `i` appears in position B
  (second) in committed comparisons

Pair repetition tracking:

- `pair_obs_count[{i,j}]`: total committed observations of unordered
  pair `{i,j}` (with canonical key `i<j`)
- `pair_obs_count_ordered[(A,B)]`: committed observations of ordered
  presentation `(A,B)`
- `pair_last_order[{i,j}]`: ordered presentation `(A,B)` used in the
  most recent **committed** comparison for unordered pair `{i,j}`
  (needed to enforce order-reversal).

Implementation note (normative intent):
`pair_obs_count[{i,j}] = pair_obs_count_ordered[(i,j)] + pair_obs_count_ordered[(j,i)]`
when `i != j`.

> **Step definition (normative).** The adaptive controller operates in
> steps. Each step attempts to select one unordered pair $\{ i,j\}$ for
> judgment, assigns presentation order $(A,B)$, submits it to the judge,
> and commits the result transactionally if valid. TrueSkill is updated
> immediately after each committed result. If the judge response is
> invalid, the step is recorded with `pair_id = NA` and no counts are
> updated.

------------------------------------------------------------------------

#### 2.2 Hard Invariants

1.  **Warm-start connectivity guarantee** The warm-start procedure
    (§3.1) must produce a connected comparison graph over all items.
    After warm-start, the graph remains connected because edges are only
    added (never removed). Therefore, “connectivity” is not used as a
    per-step candidate filter after warm-start.

    Coverage and representation are enforced via:

    - early-phase coverage quota (§3.2), and
    - star caps (§6.1), and
    - exploration routing (§7.5).

2.  **Transactional updates**  
    Comparisons are committed only if the judge returns a valid
    response. A judge response is valid iff it deterministically maps to
    Y ∈ {0,1} for the presented (A,B) (i.e., selects exactly one of A or
    B). Any other response (missing, malformed, tie, both-selected,
    refusal/timeout) is invalid. If the response is invalid or missing,
    the step is recorded with `pair_id = NA`, and no state updates
    occur: do not update TrueSkill, `deg`, `pos_count_A/B`,
    `pair_obs_count`, or any other counters or logs that are defined
    over committed comparisons. Candidate-generation diagnostics for
    that step (e.g., `n_candidates_generated`, `fallback_path`) may
    still be logged, but any fields defined only for committed
    comparisons must remain `NA` and no committed-comparison state is
    updated.

3.  **Duplicate control with enforced order reversal**

    - Repeated unordered pairs must reverse presentation order.
    - Additional repeats are allowed only for persistently ambiguous
      pairs.

4.  **Position balance**  
    Approximate 50/50 A/B exposure is enforced *after* unordered pair
    selection, except where order reversal is required by invariant (3).

------------------------------------------------------------------------

### 3. Warm‑Start Design

#### 3.1 Shuffled Chain Construction (default)

1.  Shuffle item ids using the run RNG seed

2.  Add comparisons:

    $$\lbrack\left( id_{1},id_{2} \right),\left( id_{2},id_{3} \right),\ldots,\left( id_{N - 1},id_{N} \right)\rbrack$$

**Properties**

- Guarantees connectivity
- Guarantees `min_degree ≥ 1`
- Requires exactly `N−1` comparisons
- Avoids structural artifacts from input order

#### 3.2 Early-Phase Coverage Completion

After warm start:

- Adaptive pairing begins immediately.

- A **coverage quota routing probability** is enforced until
  `min_degree ≥ 2`:

  - Define `quota_eps` (default 0.20).
  - While `min_degree < 2`, with probability `quota_eps`, the selected
    pair must include at least one endpoint from the low-degree set
    (defined in §5.1.1).

- After `min_degree ≥ 2`, the quota is disabled.

This quota is implemented step-by-step and therefore targets the stated
fraction only in expectation over steps.

------------------------------------------------------------------------

### 4. Automatic Defaults (Functions of N)

#### 4.1 Scope of Automatic Defaults

This section defines adaptive defaults that scale with the number of
items N. Unless explicitly stated otherwise, all defaults in §4 are
normative and apply globally throughout the run.

#### 4.2 Exploration Rate (Coverage-Oriented, One-Pair Steps)

The adaptive algorithm maintains an explicit exploration rate, denoted
`explore_rate`, which controls the probability that a single selection
step is routed to a coverage-oriented exploration policy rather than
pure utility maximization.

#### Definition (per step)

At each step, after candidate generation and filtering:

- With probability `explore_rate`, choose the next pair using
  exploration selection.
- With probability $1 - {explore\_ rate}$, choose the next pair using
  exploitation selection.

Exploration and exploitation are defined in §7.5.

#### Interpretation under TrueSkill pairing

Exploration is not uniform random sampling. Instead, exploration selects
pairs that preferentially include items with:

- low cumulative degree, and/or
- low recent participation (as used by star caps in §6.1),

while still pairing them using a weak TrueSkill heuristic (e.g., closest
$\mu$ among eligible partners).

This ensures exploration:

- improves coverage and connectivity,
- reduces the risk of isolated or weakly connected items,
- avoids wasting comparisons on obviously uninformative mismatches.

#### Defaults

The exploration rate is a function of $N$:

    explore_rate = clamp(0.10, 0.25, 0.20 − 0.02 * log10(N))

#### Logging

Each step must log:

- `is_explore_step` (TRUE/FALSE)
- `explore_reason` (e.g., “probabilistic”, “coverage_quota_override”)
- degree summaries of selected endpoints (e.g., `deg_i`, `deg_j`)
- whether the chosen pair was forced by a fallback stage

##### Late-stage exploration taper

When `global_identified == TRUE`, the effective exploration rate is
reduced:

    explore_rate_used = explore_rate * explore_taper_mult

**Default:**

    explore_taper_mult = 0.50

This modification applies only after global identifiability is achieved
and does not affect early-phase coverage behavior.

**Logging:** - Per step: `explore_rate_used`

------------------------------------------------------------------------

#### 4.3 Refit Cadence

Refits are triggered by increases in the average degree of the committed
comparison graph.

Let:

- $M_{\text{done}}$ be the number of completed / committed comparisons
  (i.e., with valid outcomes),
- $N$ be the number of items.

Define: $$\overline{d} = \frac{2M_{\text{done}}}{N}$$

A refit is eligible when: $$\Delta\left( \overline{d} \right) \geq 1$$

Equivalently, refit after at least:
$$\Delta M_{\text{done}} \geq \left\lceil \frac{N}{2} \right\rceil$$

new completed comparisons since the last refit, subject to clamps:

    refit_pairs_target = clamp(100, 5000, ceil(N/2))

**Operational rule:** refit triggers are evaluated using completed
comparisons only (not scheduled/inflight pairs). Warm-start comparisons
count toward $M_{\text{done}}$ and therefore toward the first refit
trigger.

#### 4.4 Effective Sample Size (ESS) Thresholds

Minimum effective sample size requirements scale with the number of
items to ensure that Monte Carlo noise remains negligible for global
quantities (rank stability, correlations, reliability) as model
dimension grows.

Define the default ESS thresholds as functions of `N`:

    ess_bulk_min =
    max(400, round(20 * sqrt(N)))

    ess_bulk_min_near_stop =
    max(1000, round(50 * sqrt(N)))

**Interpretation:**

- The `sqrt(N)` scaling increases strictness for larger problems without
  making refits prohibitively expensive.
- The lower bounds (400, 1000) ensure reasonable behavior for small `N`.
- `ess_bulk_min` applies during normal operation (Phase 2).
- `ess_bulk_min_near_stop` applies only after Phase 3 entry (§5.4).

Both thresholds are computed once per run and printed explicitly at each
refit.

------------------------------------------------------------------------

#### 4.5 Defaults Summary (normative)

| Parameter                           | Default                                                                 |
|-------------------------------------|-------------------------------------------------------------------------|
| `C_max`                             | `20000`                                                                 |
| `quota_eps`                         | `0.20`                                                                  |
| `explore_rate`                      | `clamp(0.10, 0.25, 0.20 − 0.02 * log10(N))`                             |
| `explore_resample_max`              | `10`                                                                    |
| `explore_nonlocal_rate`             | `0.15`                                                                  |
| `cap_frac`                          | `0.08`                                                                  |
| `dup_p_margin`                      | `0.05`                                                                  |
| `dup_max_obs`                       | `2`                                                                     |
| `dup_max_obs_relaxed`               | `3`                                                                     |
| `q`                                 | `0.90`                                                                  |
| `refit_pairs_target`                | `clamp(100, 5000, ceil(N/2))`                                           |
| `round_pairs_target`                | `ceil(refit_pairs_target / 2)`                                          |
| `W_cap`                             | `max(200, min(2000, refit_pairs_target))`                               |
| `ess_bulk_min`                      | `max(400, round(20 * sqrt(N)))`                                         |
| `ess_bulk_min_near_stop`            | `max(1000, round(50 * sqrt(N)))`                                        |
| `near_tie_p_low`, `near_tie_p_high` | `0.40`, `0.60`                                                          |
| `repeat_in_round_budget`            | `2`                                                                     |
| `anchor_frac_early`                 | `0.30`                                                                  |
| `anchor_frac_late`                  | `0.20`                                                                  |
| `anchor_rounds_early`               | `4`                                                                     |
| `long_frac_early`                   | `0.10`                                                                  |
| `long_frac_late`                    | `0.05`                                                                  |
| `long_rounds_early`                 | `4`                                                                     |
| `mid_frac`                          | `0.10`                                                                  |
| `k_total`                           | `clamp(10, 40, round(0.10 * N))`                                        |
| `k_top`                             | `round(0.30 * k_total)`                                                 |
| `k_bot`                             | `k_top`                                                                 |
| `k_mid`                             | `k_total - k_top - k_bot`                                               |
| `top_band_pct`                      | `0.10`                                                                  |
| `top_band_bins`                     | `5`                                                                     |
| `K_base`                            | `5` if `N<60`; `10` if `60≤N<150`; `12` if `150≤N<400`; `20` if `N≥400` |
| `long_min_dist`                     | `ceil(0.5 * K_base)`                                                    |
| `mid_min_dist`                      | `2`                                                                     |
| `mid_max_dist`                      | `min(4, K_base - 1)`                                                    |

#### 4.5.1 Global Identifiability State

The adaptive controller maintains a derived Boolean state indicating
whether the global latent scale is sufficiently identified to permit
aggressive local refinement.

Define, at each refit:

    global_identified =
    (reliability_EAP ≥ global_identified_reliability_min)
    AND
    (ts_btl_rank_spearman ≥ global_identified_rank_corr_min)

**Defaults (normative):**

    global_identified_reliability_min = 0.80
    global_identified_rank_corr_min   = 0.90

**Interpretation:**

When `global_identified = TRUE`, posterior uncertainty is no longer
dominated by global scale ambiguity. Subsequent adaptive sampling should
prioritize local discrimination and boundary refinement rather than
long-range connectivity.

This state affects: - round-stage quota allocation (§4.6.2), - long-link
eligibility (§5.1.2), - exploration rate (§4.2), - and star-cap
overrides (§6.1.1).

**Logging:**

Each refit must log: - `global_identified` (TRUE/FALSE) - the threshold
values used.

#### 4.6 Round Cadence and Quotas (normative)

Selection is organized into rounds. A round is a planning construct that
schedules a fixed number of committed comparisons (valid judge outcomes
only). Pair selection still operates step-by-step (§7), but each step is
assigned to a round stage (§7.2).

##### 4.6.1 Round size (committed comparisons)

Let: - `refit_pairs_target = clamp(100, 5000, ceil(N/2))` (§4.3)

Define:

    round_pairs_target = ceil(refit_pairs_target / 2)

Design intent: approximately 2 rounds per refit.

Rounds count committed comparisons only. Invalid judge outcomes do not
advance the round’s committed-pair counter (§2.2 Invariant 2).

##### 4.6.2 Round stage quotas

Each round has four pair-type stages with target committed counts:

1.  `anchor_link`
2.  `long_link`
3.  `mid_link`
4.  `local_link`

Base quotas are computed as:

    anchor_quota = ceil(anchor_frac * round_pairs_target)
    long_quota   = ceil(long_frac   * round_pairs_target)
    mid_quota    = ceil(mid_frac    * round_pairs_target)
    local_quota  = round_pairs_target - (anchor_quota + long_quota + mid_quota)

#### Time-varying base fractions

- For rounds `round_id ≤ anchor_rounds_early`:
  - `anchor_frac = anchor_frac_early`
- Else:
  - `anchor_frac = anchor_frac_late`
- For rounds `round_id ≤ long_rounds_early`:
  - `long_frac = long_frac_early`
- Else:
  - `long_frac = long_frac_late`
- `mid_frac` is constant.

#### Global-identifiability taper

If `global_identified == TRUE` at the most recent refit (§4.5.1):

    long_frac_effective =
    max(long_frac_floor, long_frac * long_taper_mult)

**Defaults (normative):**

    long_taper_mult = 0.25
    long_frac_floor = 0.02

Define:

    long_quota_raw = ceil(long_frac * round_pairs_target)

    long_quota_effective =
        ceil(long_frac_effective * round_pairs_target)

    long_quota_removed =
        max(0, long_quota_raw - long_quota_effective)

    realloc_to_mid =
        ceil(mid_bonus_frac * long_quota_removed)

    realloc_to_local =
        long_quota_removed - realloc_to_mid

Apply reallocation:

    mid_quota   += realloc_to_mid
    local_quota += realloc_to_local

#### Design intent

After global scale identification, long-range links yield diminishing
marginal information. v3.1 explicitly reallocates this budget to
higher-yield local and mid-range refinement while retaining a minimal
long-link trickle to preserve graph robustness.

#### Logging

At the refit following each completed round, the following fields must
be logged: - `long_quota_raw` - `long_quota_effective` -
`long_quota_removed` - `realloc_to_mid` - `realloc_to_local`

------------------------------------------------------------------------

### 5. Candidate Generation and Feasibility Safeguards

### 5.0 Rank Strata and Percentile Top-Band (normative)

Candidate generation is done using rank strata that can scale to N up to
2000 while supporting: - local refinement, - mid/long links, and - a
percentile-based top band for finer control near the top of the scale.

All strata are defined over the current ordering `rank_mu` (TrueSkill μ)
with deterministic tie-breaking: - primary: μ descending - tie-break:
item_id ascending

Let `r(x)` be the 1..N rank index of item x in `rank_mu`.

#### 5.0.1 Base strata (global)

Define `K_base` strata for the full rank list.

Normative default: - N \< 60: `K_base = 5` - 60 ≤ N \< 150:
`K_base = 10` - 150 ≤ N \< 400: `K_base = 12` - N ≥ 400: `K_base = 20`

Items are assigned to equal-count bins by rank index (approximately
N/K_base per stratum; last stratum may differ by ≤1).

#### 5.0.2 Percentile-based top band (required for large N)

To support decision-relevant resolution near the top, there is a top
band defined by percentile and that is subdivided more finely than the
rest.

Parameters: - `top_band_pct` (default 0.10): top 10% of items by
`rank_mu` - `top_band_bins` (default 5): subdivide the top band into 5
equal-count bins (each ~2% of items)

Construction: 1) Let `T = ceil(top_band_pct * N)` be the count in the
top band. 2) Split ranks 1..T into `top_band_bins` equal-count bins
(±1). 3) The remaining ranks (T+1..N) are split into `K_base` strata as
in 5.0.1.

This yields: - fine-grained “top percentiles” strata for top-k boundary
control, - coarse-but-scalable strata for the remainder.

For N ≥ 400, implementations should prefer computing strata from rank
indices directly (no O(N^2) pair enumeration) and generate candidates
via sampled partner pools per item, subject to `C_max`.

#### 5.0.3 Stratum distance

Define: - `dist_stratum(i,j) = |stratum(i) - stratum(j)|`

Pair-type stages impose constraints on `dist_stratum` (§7.2).

### 5.1 Candidate Pools (normative; stage-specific)

Candidates are generated according to the active round stage. After
generation, all candidates pass through the canonical pipeline (§5.2.1).

#### 5.1.1 Stage: anchor_link

Anchor-link candidates are unordered pairs `{i,j}` where: -
`i ∈ anchors` (rolling anchor set; §7.1), and - `j ∉ anchors`.

Partner targeting is stage-specific but must respect all hard
invariants, duplicate rules, and star caps via the canonical pipeline.

#### 5.1.2 Stage: long_link

Generate unordered candidates `{i,j}` such that:

- `i` and `j` are non-anchors, and
- `dist_stratum(i,j) ≥ long_min_dist`.

**Additional posterior gate:**

When `global_identified == TRUE`, a long-link candidate `{i,j}` is
eligible only if:

    p_ij_posterior ∈ [p_long_low, p_long_high]

**Defaults (normative):**

    p_long_low  = 0.10
    p_long_high = 0.90

Where `p_ij_posterior` is the posterior win probability
`Pr(θ_i > θ_j | posterior)` computed at the most recent refit.

**Fallback rule:** If posterior probabilities are unavailable (e.g.,
prior to the first refit), use the TrueSkill proxy `p_ij`.

**Interpretation:**

This gate prevents expending long-range comparisons on pairs that are
already effectively decided once global scale has stabilized.

**Logging:**

Each attempted long-link step must log: - `long_gate_pass`
(TRUE/FALSE) - `long_gate_reason` (e.g., `posterior_extreme`,
`posterior_unavailable`)

#### 5.1.3 Stage: mid_link

Generate unordered candidates `{i,j}` such that: - `i` and `j` are
non-anchors, and - `mid_min_dist ≤ dist_stratum(i,j) ≤ mid_max_dist`

Defaults:

    mid_min_dist = 2
    mid_max_dist = 4

If K_base \< 10, clamp mid_max_dist to K_base-1.

#### 5.1.4 Stage: local_link

Generate unordered candidates `{i,j}` such that: - both endpoints are
eligible for local selection (anchors allowed or disallowed by config;
default allow), - and either: - same stratum, or - adjacent stratum
(`dist_stratum ≤ 1`)

Optional local enrichment (recommended for small N): - If local
candidate count is below a small threshold, allow `dist_stratum ≤ 2`
before invoking the fallback ladder.

##### 5.1.4.1 Local-link prioritization

When `global_identified == TRUE`, local-link selection is biased toward
pairs that resolve remaining posterior ambiguity.

Within the eligible local candidate set:

Priority is given to pairs satisfying one or more of: - near-tie
condition:

    |p_ij − 0.5| minimal

- boundary refinement: items whose current EAP ranks lie within a window
  around the top-k cutoff. “Current EAP ranks” refers to the most recent
  refit’s EAP ranking when available; otherwise use the proxy ranking
  `rank_mu`.

**Defaults (normative):**

    boundary_k = 20
    boundary_window = max(10, ceil(0.05 * N))
    boundary_frac = 0.15   # fraction of local quota

This prioritization affects only utility ranking within the local stage
and does not override invariants, duplicate rules, or star caps.

**Logging:** - `local_priority_mode ∈ {standard, near_tie, boundary}`

#### 5.1.5 Candidate cap (C_max)

All generated candidate sets are capped at:

    C_max = 20,000 unordered pairs per selection attempt

If the set exceeds `C_max`, subsample uniformly without replacement
using a stage-specific deterministic seed derived from: - run seed -
`step_id` - stage id (anchor/long/mid/local) - fallback stage id (if
applicable)

Uniform subsampling is normative: do not subsample by utility or rank
position.

#### 5.1.6 Per-round exposure constraint (Pollitt-style; soft)

Within a round, the goal is to have ≤1 appearance per item.

Normative policy (“soft Pollitt”): - disallow selecting an item already
used in the current round, - except permit up to
`repeat_in_round_budget` total repeated item-uses per round (default
2), - repeated-use is allowed only for items in the bottom quartile of
`recent_deg` (under-represented) and only after stage-local candidate
starvation would otherwise occur.

This rule is applied as an additional hard filter within candidate
generation for the active stage (before duplicate and star-cap filters),
because it operates at the round-plan level.

------------------------------------------------------------------------

#### 5.2 Candidate Starvation Detection

Within a step, the controller may attempt multiple fallback stages
(§5.3). A step is considered starved only if no valid pair is selected
after exhausting all fallback stages.

Define:

    candidate_starved = (no valid pair selected after all fallback stages)

Per-step logs must also record the first attempt that succeeded (if any)
via `fallback_used` and `fallback_path`.

Note: `candidate_starved = TRUE` at the step level indicates failure
after exhausting all fallback stages for the current step. Run
termination occurs only if all round stages are exhausted without
producing a valid pair (§7.7).

------------------------------------------------------------------------

#### 5.2.1 Canonical candidate pipeline (normative)

Within any selection attempt (base or fallback stage), candidates must
be processed in the following order:

1.  **Generate** an unordered candidate set `C` (deduplicated unordered
    pairs).
2.  **Compute** TrueSkill quantities for each `{i,j}` in `C`: `p_ij` and
    `U0_ij`.
3.  **Apply hard filters**:
    - remove self-pairs,
    - remove pairs that violate any hard invariant (including enforced
      order-reversal feasibility for repeats).
4.  **Apply duplicate policy** (default or relaxed depending on stage).
5.  **Apply star caps** (§6.1.1) as an eligibility filter.
6.  **Apply optional continuous degree regularization** (if enabled) to
    produce final utility `U_ij`.
7.  **Select** the next unordered pair using exploration/exploitation
    (§7.5).

Logging counts must refer to these pipeline points:

- `n_candidates_generated`: number of unordered pairs after Step 1
- `n_candidates_after_hard_filters`: after Step 3
- `n_candidates_after_duplicates`: after Step 4
- `n_candidates_after_star_caps`: after Step 5
- `n_candidates_scored`: after Step 6 (equals
  `n_candidates_after_star_caps` if regularization is off)

`n_candidates_after_filters` (if kept) must equal
`n_candidates_after_star_caps`.

------------------------------------------------------------------------

### 5.3 Feasibility Fallback Ladder (within-stage)

Candidate generation can fail within a stage due to star caps,
duplicates, round exposure constraints, or thin strata.

The fallback ladder is applied within the active round stage first. If
all within-stage fallback attempts fail, the stage is declared starved
and the controller proceeds to the next round stage (§7.2.5).

Hard invariants are never relaxed.

#### 5.3.1 Within-stage fallback stages (in order)

For a given step under a given round stage, attempt:

1.  **base**: stage’s default candidate rule (§5.1)
2.  **expand_locality**: widen locality *within the stage definition*
    (e.g., local dist_stratum threshold from ≤1 to ≤2; mid range from
    2–4 to 1–5)
3.  **uncertainty_pool**: keep stage constraints but temporarily
    increase exploration routing for this step:
    - `explore_rate_used = min(0.50, 2 * explore_rate)`
    - Note: this temporary increase applies to explore_rate_used, not
      the base explore_rate.
4.  **dup_relax**: same as uncertainty_pool but allow relaxed duplicates
    (§5.3.2)
5.  **global_safe**: as final within-stage backstop, enumerate all
    unordered pairs that satisfy the stage’s type constraint
    (anchor/non-anchor and distance band) with minimal locality
    restrictions

If still no valid pair is found: - mark step as `candidate_starved=TRUE`
for that attempt and return control to the round controller to move to
the next stage.

Logging must record: - active round stage - within-stage fallback_used
and fallback_path - whether stage starvation occurred

##### 5.3.2 Duplicate allowance rule (normative; TrueSkill-based)

Duplicate policy is applied at pipeline Step 4 (§5.2.1) and depends on
stage:

- **Default policy**: allow at most `dup_max_obs` committed observations
  per unordered pair.
- **Relaxed policy** (only in stage `dup_relax`): allow at most
  `dup_max_obs_relaxed`.

For an unordered pair `{i,j}` with current TrueSkill win probability
`p_ij` and base utility `U0_ij`, repeats beyond the second committed
observation are allowed **only** if all conditions hold:

1.  Persistent ambiguity (TrueSkill-based):
    $$\left| p_{ij} - 0.5 \right| \leq \text{dup\_p\_margin}$$

2.  High utility relative to the *eligible* candidate set of the current
    attempt:

    Let `C_eligible` be the candidate set **after hard filters and star
    caps**, but **before** applying duplicate relaxation logic for this
    pair. Define:
    $$U_{\text{dup\_threshold}} = \text{Quantile}_{q}\{ U0_{uv}:\{ u,v\} \in C_{\text{eligible}}\}$$
    Require: $$U0_{ij} \geq U_{\text{dup\_threshold}}$$

3.  Observation cap:

&nbsp;

    pair_obs_count[{i,j}] < dup_max_obs # default policy
    pair_obs_count[{i,j}] < dup_max_obs_relaxed # relaxed policy (dup_relax only)

**Defaults:**

    dup_p_margin = 0.05 # repeats allowed when p_ij ∈ [0.45, 0.55]
    dup_max_obs = 2 # max committed observations per unordered pair under default policy
    dup_max_obs_relaxed = 3 # max committed observations per unordered pair in dup_relax stage
    q = 0.90 # utility quantile threshold

**Order invariants:** Order-reversal is always enforced for repeated
unordered pairs (§7.6), regardless of duplicate policy.

##### Logging requirements (audit)

At minimum, the per-step log must record:

- `candidate_starved`
- `fallback_used` and full `fallback_path` (e.g.,
  `base>expand_locality>dup_relax`)
- `starvation_reason` (best-effort classification; `NA` if not starved)

Additionally, the implementation must internally capture per-attempt
counts (generated/surviving) to support debugging, but only the
step-level fields above are required in the public `step_log` schema.

#### 5.4 Phase 3 Entry (Near-Stop Behavior)

Phase 3 is intended to tighten numerical accuracy requirements as the
run approaches stopping.

##### Entry criterion (evaluated only at refits)

Phase 3 is entered at refit `t` if:

    Rel_EAP(t) ≥ (eap_reliability_min − 0.05)

and the MCMC diagnostics gate passes using the **Phase 2 ESS threshold**
(`ess_bulk_min`).

##### Normative behavior in Phase 3

Phase 3 modifies **only** the diagnostics strictness used at refits.

In Phase 3:

- the posterior is considered usable only if:

&nbsp;

    min_ess_bulk ≥ ess_bulk_min_near_stop

- all other aspects of the adaptive algorithm are unchanged:
  - pair selection,
  - exploration/exploitation routing,
  - star caps,
  - fallback ladder,
  - logging behavior.

This ensures that near stopping, Monte Carlo error is negligible
relative to true posterior uncertainty.

------------------------------------------------------------------------

### 6. Utility Function (TrueSkill-based)

For unordered $\{ i,j\}$, each item has:

- mean $\mu_{i}$
- uncertainty $\sigma_{i}$

Let:

$$d_{ij} = \mu_{i} - \mu_{j}$$

$$s_{ij}^{2} = \sigma_{i}^{2} + \sigma_{j}^{2} + 2\beta^{2}$$

Define the implied win probability:

$$p_{ij} = \Phi\left( \frac{d_{ij}}{\sqrt{s_{ij}^{2}}} \right)$$

Define base utility:

$$U_{0}(i,j) = p_{ij}\left( 1 - p_{ij} \right)$$

This peaks at maximal outcome uncertainty.

------------------------------------------------------------------------

### **6.1 Degree Control and Star Caps**

Under TrueSkill pairing:

- uncertainty (σ) naturally shrinks for frequently sampled items,
- aggressive degree regularization is unnecessary and can be overly
  conservative.

Therefore, degree control is implemented using:

1.  early-phase coverage constraints (§3.2), and
2.  star caps (this section), rather than strong continuous penalties in
    the utility function.

------------------------------------------------------------------------

#### 6.1.1 Star Caps (default enabled)

A **star cap** limits how frequently any single item may appear in
recent comparisons, preventing “star nodes” that dominate the sampling
budget.

##### Definition

Maintain a rolling window of the most recent completed comparisons:

    W_cap = max(200, min(2000, refit_pairs_target))

where `refit_pairs_target` is defined in §4.3.

For each item (i), define:

    recent_deg[i] = number of appearances of i in the last W_cap comparisons

A candidate unordered pair `{i,j}` is **ineligible** if either endpoint
violates the cap:

    recent_deg[i] > cap_count  OR  recent_deg[j] > cap_count

Where:

    cap_count = ceil(cap_frac * W_cap)

##### Defaults

    cap_frac = 0.08

This ensures that no item participates in more than approximately 8% of
recent comparisons.

**Interpretation:**

- Prevents runaway focus on boundary items
- Preserves adaptivity while enforcing fairness
- Scales automatically with run length and refit cadence

##### Conditional near-tie override

When `global_identified == TRUE`, a star-cap rejection may be overridden
only if:

1.  the pair is a near-tie:

&nbsp;

    |p_ij − 0.5| ≤ p_star_override_margin

2.  both endpoints have acceptable total exposure (`deg` not extreme),
3.  and the per-round override budget is not exceeded.

**Defaults:**

    p_star_override_margin = 0.05
    star_override_budget_per_round = 1

Overrides apply only to local-link and mid-link stages.

**Logging (per step):** - `star_override_used` (TRUE/FALSE) -
`star_override_reason`

------------------------------------------------------------------------

#### 6.1.2 Interaction with Utility

Operationally, star caps are applied as the eligibility filter at
pipeline Step 5 (§5.2.1), after computing `p_ij` and base utility
`U0_ij`, and before any optional continuous degree regularization that
produces `U_ij`.

Star caps: \* do not modify utility values, \* do not affect candidate
generation, \* do not override hard invariants.

If star caps eliminate too many high-utility pairs, the standard
fallback ladder (§5.3) applies unchanged.

------------------------------------------------------------------------

#### 6.1.3 Optional Continuous Degree Regularization (off by default)

Continuous degree regularization may be enabled only as a secondary
guardrail if star behavior persists.

If enabled, it must satisfy all of the following:

- **Gated**: applied only when both items have `deg ≥ 2`
- **Weak**: exponent α ≤ 0.1
- **Capped**: maximum penalty multiplier ≤ 2–3×

Form:

$$U(i,j) = \frac{U_{0}(i,j)}{\left( \left( deg_{i} + 1 \right)\left( deg_{j} + 1 \right) \right)^{\alpha}}$$

This option is disabled by default and should be activated only with
explicit justification.

------------------------------------------------------------------------

#### 6.1.4 Logging and Audit

The following star-cap diagnostics must be recorded:

**Per step (`step_log`):**

- `star_cap_rejects`
- `star_cap_reject_items` (count of unique items triggering rejections)

**Per refit (`round_log`):**

- total star-cap rejects since last refit
- proportion of candidate rejections due to star caps
- max and median `recent_deg`

These fields allow retrospective analysis of whether star caps were
binding and whether they materially influenced pairing behavior.

------------------------------------------------------------------------

### 7. Pair Selection Procedure (Round Controller + Step Engine)

Pair selection still occurs as one-pair steps (§2.1 Step definition),
but steps are scheduled by a round controller that enforces
Pollitt-style exposure and budgets global linking.

#### 7.1 Rolling anchors (normative)

Anchors are defined from the current ranking proxy and refreshed at
refit time (default) or optionally every round.

Normative default: refresh anchors every refit using BTL EAP means when
available; otherwise TrueSkill μ.

Anchor set size: - `k_total = clamp(10, 40, round(0.10 * N))` -
`k_top = round(0.30 * k_total)` - `k_bot = k_top` -
`k_mid = k_total - k_top - k_bot`

Anchors are selected as: - `k_top` highest-ranked items, - `k_bot`
lowest-ranked items, - `k_mid` centered around the median rank.

Anchors are used only for scheduling; they do not alter stopping
criteria.

#### 7.2 Round controller (normative)

This section describes execution of the round plan defined in §4.6; it
does not redefine quotas or cadence.

Each round plans `round_pairs_target` committed comparisons (§4.6.1) and
proceeds through stages in order:

1.  anchor_link until `anchor_quota` committed pairs
2.  long_link until `long_quota` committed pairs
3.  mid_link until `mid_quota` committed pairs
4.  local_link until `local_quota` committed pairs

Within each stage, each committed comparison is produced by executing
the standard step engine:

- generate candidates per stage (§5.1),
- apply the canonical pipeline (§5.2.1) unchanged,
- route to exploration vs exploitation per rules (§7.5),
- assign order with reversal and position-balance (§7.6),
- commit transactionally (§2.2 invariant 2).

#### 7.2.1 Stage-specific partner targeting (selection scoring)

Stage constraints define the candidate set. Within that set, apply the
selection rules: - exploitation: choose max utility (U0 or U if
regularization enabled) - exploration: use exploration logic, but
constrained to the stage’s candidate set

#### 7.2.2 Handling stage starvation (normative)

If a stage cannot produce valid pairs after exhausting its within-stage
fallback ladder (§5.3), the round controller: - logs stage starvation
and the shortfall count, - proceeds to the next stage.

Stages are not revisited within the same round.

#### 7.2.3 Soft Pollitt exposure enforcement

Within a round, items are intended to appear at most once, but v3
permits a small repeat budget (§5.1.6) to prevent deadlocks.

#### 7.2.4 Refit cadence and continuity

Refits are triggered exactly as in §4.3, using committed pairs only. The
existence of rounds does not change refit eligibility.

------------------------------------------------------------------------

#### 7.3 Degree control is enforced via the canonical pipeline (normative)

Degree control is not a separate selection phase. It is enforced *only*
through the canonical candidate pipeline (§5.2.1), which fixes the order
of computation and filtering for every stage and fallback attempt.

**Normative implementation rule:** - Star caps (§6.1.1) MUST be applied
as an eligibility filter at pipeline Step 5 (§5.2.1), after computing
`p_ij` / `U0_ij` and after duplicate policy, and before any optional
continuous degree regularization.

Accordingly: - Star caps do not modify `U0_ij` values; they only remove
ineligible candidates. - If star caps eliminate too many candidates, the
fallback ladder (§5.3) applies unchanged.

##### 7.3.1 Star caps (default enabled)

Star caps are defined in §6.1.1. This section adds only the operational
requirement that the step log captures star-cap effects at the pipeline
boundary:

**Logging (per step; required):** - `star_cap_rejects` -
`star_cap_reject_items`

If `global_identified == TRUE`, conditional star-cap override behavior
(§6.1.1) may apply. Override usage and reasons must be logged:

**Logging (per step; required):** - `star_override_used` -
`star_override_reason`

##### 7.3.2 Optional continuous degree regularization (off by default)

If continuous degree regularization is enabled, it MUST be applied only
after star-cap filtering (pipeline Step 6), using:

$$U(i,j) = \frac{U_{0}(i,j)}{\left( \left( {deg}_{i} + 1 \right)\left( {deg}_{j} + 1 \right) \right)^{\alpha}}$$

subject to: - **Gated**: applied only when both endpoints have
`deg ≥ 2` - **Weak**: `α ≤ 0.1` - **Capped**: maximum penalty multiplier
≤ 2–3×

When regularization is off (default), the final scoring utility is
`U_ij = U0_ij`.

**Logging (per step; required):** - `n_candidates_scored` must equal
`n_candidates_after_star_caps` when regularization is off.

------------------------------------------------------------------------

#### 7.4 Filter by invariants and duplicate policy (normative)

Candidates are removed only for the following reasons:

**Hard invariant filters** - self-pairs (i = j) are forbidden, -
transactional constraints (no partial commit; see §2.2 Invariant 2), -
order-reversal feasibility for repeats (see §7.6): if `{i,j}` has been
committed before, only the required next order is feasible.

**Duplicate policy** - Apply default or relaxed duplicate caps as
specified by the active stage (§5.3.2).

**Non-hard preferences (not filters)** - Position balance is a
best-effort assignment at ordering time (§7.6) and must not be used to
exclude an unordered pair.

------------------------------------------------------------------------

##### Under-represented set (normative)

Define an under-represented set `underrep_set` for use by coverage quota
routing (§3.2) and exploration selection (§7.5.2):

- Let `deg[i]` be total committed degree.
- Let `recent_deg[i]` be rolling-window participation (§6.1.1).

Define: - `D_min = min_i deg[i]` -
`underrep_set = { i : deg[i] ≤ D_min + 1 }`

If `underrep_set` is empty due to numerical issues, set `underrep_set`
to all items.

Implementation note: this definition is intentionally simple and stable;
star caps already control extreme recent imbalances.

#### 7.5 Select the next pair (exploitation vs exploration)

Selection chooses exactly one pair at each step.

##### 7.5.1 Coverage quota override (early phase)

While `min_degree < 2`, enforce the early-phase coverage quota (§3.2) as
a **stepwise rule**:

- With probability `quota_eps` (default 0.20), the selected pair must
  include a low-degree endpoint.
- Otherwise proceed normally.

This rule is disabled after `min_degree ≥ 2`.

##### 7.5.2 Exploration routing (per step)

If the quota override does not apply:

- With probability `explore_rate_used`, perform **exploration
  selection**.
- With probability $1 - \text{explore\_rate\_used}$, perform
  **exploitation selection**.

`explore_rate_used` equals `explore_rate` unless
`global_identified == TRUE`, in which case it is tapered per §4.2.

All exploration/exploitation selection is performed over the candidate
set defined by the active `round_stage` (i.e., the stage constraint is
applied before exploration partner choice).

##### Exploration selection (with non-local connections)

Exploration is designed to improve representation and occasionally
create long-range constraints.

Exploration proceeds as follows:

1.  Choose an endpoint `i` uniformly at random from `underrep_set`
    (§7.4).

2.  Decide exploration mode:

    - With probability `explore_nonlocal_rate`, use **non-local
      exploration**.
    - Otherwise use **local exploration**.

Let `r(x)` denote the 1..N rank index of item `x` in `rank_mu`.

3.  Define eligible partners: `J(i)` = all `j` such that `{i,j}`
    survives the canonical pipeline through Step 5 (§5.2.1), i.e., hard
    filters + duplicate policy + star caps.

4.  Select partner:

**Local exploration (default):** - Choose `j ∈ J(i)` that minimizes
`|μ_i - μ_j|`. - Tie-breaker 1: maximize `U0(i,j)` - Tie-breaker 2:
minimize `recent_deg[j]` - Tie-breaker 3: deterministic by `item_id`

**Non-local exploration:** - Choose `j ∈ J(i)` that maximizes rank
distance in `rank_mu`: - maximize `|r(i) - r(j)|` - Tie-breaker 1:
maximize `U0(i,j)` (avoid obviously uninformative extremes) -
Tie-breaker 2: minimize `recent_deg[j]` - Tie-breaker 3: deterministic
by `item_id`

If no valid partner exists for sampled `i`, resample `i` up to
`explore_resample_max` times (default 10). If still no valid pair is
found, invoke the fallback ladder (§5.3), and only set
candidate_starved=TRUE if the ladder exhausts.

**Defaults:**

    explore_resample_max = 10
    explore_nonlocal_rate = 0.15

**Logging:** Each exploration step must log
`explore_mode ∈ {local, nonlocal}`.

##### Exploitation selection

- Select the single highest-utility remaining pair (by $U_{0}$ or $U$),
  subject to invariants, star caps, and duplicate constraints.

##### Precedence rule

Coverage quota and exploration routing do not override hard invariants,
duplicate rules, or star caps. If the routed policy (quota or
exploration) cannot produce a valid pair, the step invokes the fallback
ladder (§5.3). If all fallback stages fail, the run stops with
`candidate_starvation`.

------------------------------------------------------------------------

#### 7.6 Assign presentation order (A/B) and enforce order invariants (normative)

After selecting an unordered pair `{i,j}`, assign ordered presentation
`(A,B)` using this priority:

1.  **Order-reversal invariant (hard):** If
    `pair_obs_count[{i,j}] >= 1`, the next committed observation must
    reverse the most recent committed order (from pair_last_order) for
    this unordered pair.
    - If last committed order was `(i,j)`, require `(j,i)` next.
    - If last committed order was `(j,i)`, require `(i,j)` next.
2.  **Otherwise, position balance (best-effort):** Choose the order that
    reduces the absolute per-item imbalance:
    - define `imbalance(x) = pos_count_A[x] - pos_count_B[x]`
    - prefer assigning `x` to position A when `imbalance(x)` is smaller
      (more negative) than its partner’s.
3.  **Deterministic tie-break:** If still tied, set `A = min(i,j)` and
    `B = max(i,j)` (or any fixed deterministic rule).

Position assignment does not affect utility and must never invalidate an
unordered selection.

------------------------------------------------------------------------

#### 7.7 Fallback ladder if starved

If no valid pair can be selected at the current step:

- invoke the fallback ladder (§5.3) within the step,
- attempt progressively relaxed stages,
- if all stages fail, stop with reason `candidate_starvation`.

All fallback activity and reasons are logged.

------------------------------------------------------------------------

### 8. MCMC Configuration and Diagnostics

#### 8.1 Chains and Parallelism

- Default chains: `min(8, physical_cores)`
- Parallel chains: `min(chains, core_budget)`

All values must be printed at refit time.

------------------------------------------------------------------------

#### 8.2 Diagnostics Gate

Posterior is usable only if:

- Divergences = 0
- Max R‑hat ≤ 1.01
- Min bulk ESS ≥ applicable threshold

Where the applicable threshold is:

- `ess_bulk_min` before Phase 3 entry
- `ess_bulk_min_near_stop` after Phase 3 entry (§5.4)

------------------------------------------------------------------------

#### 8.3 Reliability (EAP)

EAP reliability is computed as:
$$\text{Rel}_{EAP} = \frac{{Var}\left( {\mathbb{E}}\left\lbrack \theta_{i} \right\rbrack \right)}{{Var}\left( {\mathbb{E}}\left\lbrack \theta_{i} \right\rbrack \right) + {\mathbb{E}}\left\lbrack {Var}\left( \theta_{i} \right) \right\rbrack}$$

**Interpretation:** proportion of total variance attributable to true
between‑item differences.

------------------------------------------------------------------------

### 9. Stopping Criteria

Stopping is based on posterior reliability and global stability. The
algorithm stops only when the posterior is diagnostically sound,
reliable, and stable across refits.

Stopping is evaluated at BTL refit rounds only and occurs immediately
when all required gates pass at an eligible refit.

------------------------------------------------------------------------

### 9.1 MCMC Diagnostics Gate

All posterior quantities used by the adaptive algorithm—including
ranking, uncertainty estimates, utilities, reliability, and stopping
criteria—are considered valid only if the MCMC sampler passes a strict
diagnostics gate. This gate ensures that the posterior has been explored
accurately and that numerical artifacts do not influence downstream
decisions.

The diagnostics gate requires all three of the following conditions to
hold.

------------------------------------------------------------------------

#### 9.1.1 Divergences (must be zero)

**Definition** A divergence is reported when Stan’s Hamiltonian Monte
Carlo (HMC) sampler encounters regions of the posterior geometry that it
cannot traverse accurately. Divergences typically arise from:

- sharp curvature in the posterior,
- funnel-like geometries,
- or insufficient numerical resolution during integration.

**Rule**

``` text
Number of divergences = 0
```

**Interpretation:** Divergences indicate that some regions of the
posterior may be systematically under-sampled, even if other diagnostics
appear acceptable. When divergences are present, posterior means,
variances, and tail probabilities may be biased.

**Design stance:** Any nonzero divergence count invalidates the
posterior for adaptive decision-making and stopping. Sampling must
continue or be corrected before results are trusted.

------------------------------------------------------------------------

#### 9.1.2 R-hat (Gelman–Rubin convergence statistic)

**Definition** R-hat compares:

- variability *within* each MCMC chain
- variability *between* chains

If all chains have converged to the same target distribution, these
quantities should agree.

Formally, values close to 1 indicate convergence: \[ \]

**Rule**

``` text
max R-hat ≤ 1.01
```

**Interpretation**

- ( \> 1.01): chains have not fully mixed or converged
- ( ): chains are sampling from the same posterior distribution

The threshold of 1.01 is intentionally conservative, reflecting the fact
that ranking stability and stopping criteria can be sensitive to small
posterior inaccuracies.

------------------------------------------------------------------------

#### 9.1.3 Effective Sample Size (ESS)

**Definition**

MCMC samples are autocorrelated. The effective sample size (ESS)
measures how many independent draws the correlated chain is equivalent
to.

Stan reports multiple ESS measures. This design uses bulk ESS, which
governs the accuracy of posterior means, variances, and correlations.

------------------------------------------------------------------------

**Rule (normative)**

At refit `t`, define the required minimum bulk ESS:

    ESS_required(t) =
    ess_bulk_min if Phase 3 has not been entered
    ess_bulk_min_near_stop if Phase 3 has been entered

The diagnostics gate requires:

``` text
min bulk ESS ≥ ESS_required(t)
```

**Interpretation**

Bulk ESS below threshold indicates that Monte Carlo noise may still
meaningfully affect: - EAP estimates, - reliability calculations, -
lagged stability correlations, - stopping decisions.

Scaling ESS requirements with N ensures comparable numerical reliability
across problem sizes.

**Logging**

Each refit must log: - `min_ess_bulk` - `ess_bulk_required` so that
diagnostics decisions are fully auditable.

------------------------------------------------------------------------

#### 9.1.4 Why all three diagnostics are required

Each diagnostic detects a distinct failure mode:

| Diagnostic  | Detects                                   |
|-------------|-------------------------------------------|
| Divergences | Posterior geometry / bias problems        |
| R-hat       | Lack of convergence or chain disagreement |
| ESS         | Excess Monte Carlo noise                  |

All three conditions must pass to ensure that:

- posterior means are accurate,
- uncertainty estimates are trustworthy,
- ranking stability metrics are meaningful,
- stopping decisions are defensible.

------------------------------------------------------------------------

#### 9.1.5 Operational rule

> **No diagnostics, no decisions.**

If any diagnostic fails:

- the posterior is treated as provisional,
- adaptive sampling may continue,
- but reliability checks, uncertainty gates, and stopping criteria are
  suspended until diagnostics pass.

This guarantees that all reported rankings and termination decisions are
based on numerically sound Bayesian inference, not sampling artifacts.

------------------------------------------------------------------------

#### 9.2 Reliability Gate (EAP)

Bayesian EAP reliability is computed as defined in §8.3:

$$\text{Rel}_{EAP}(t) = \frac{{Var}\left( {\mathbb{E}}\left\lbrack \theta_{i} \mid \text{data} \right\rbrack \right)}{{Var}\left( {\mathbb{E}}\left\lbrack \theta_{i} \mid \text{data} \right\rbrack \right) + {\mathbb{E}}\left\lbrack {Var}\left( \theta_{i} \mid \text{data} \right) \right\rbrack}$$

##### Condition

    Rel_EAP(t) ≥ eap_reliability_min

##### Logged flag

- `eap_pass` = TRUE if the condition holds at refit *t*, FALSE otherwise

##### Interpretation

When this gate passes, the majority of observed variation in item
estimates reflects **true between-item differences**, not posterior
uncertainty.

------------------------------------------------------------------------

#### 9.3 Theta Stability Gate (Lagged)

##### Eligibility

Lagged stability metrics are evaluated only when a valid lagged
comparison exists.

Specifically, the theta stability gate is eligible at refit *t* if:

    t > stability_lag

When ineligible, all lagged metrics and pass flags are recorded as `NA`.

##### Definitions

Let:

- ${\widehat{\theta}}^{(t)}$ be the vector of item **EAP** estimates at
  refit $t$
- $L = \text{stability\_lag}$

Compute:

1.  **Lagged theta correlation**
    $$\rho_{\theta}(t) = \operatorname{cor}({\widehat{\theta}}^{(t)},\,{\widehat{\theta}}^{(t - L)})$$

2.  **Relative change in theta spread**
    $$\Delta SD_{\theta}(t) = \frac{|SD\left( {\widehat{\theta}}^{(t)} \right) - SD\left( {\widehat{\theta}}^{(t - L)} \right)|}{SD\left( {\widehat{\theta}}^{(t - L)} \right)}$$

Implementation notes (normative for reproducibility):

- Correlations must use `use="pairwise.complete.obs"`.
- If `SD(θ^(t-L)) = 0`, set `ΔSD_theta(t) = NA` and
  `delta_sd_theta_pass = NA`.

##### Conditions

    rho_theta(t) ≥ theta_corr_min
    ΔSD_theta(t) ≤ theta_sd_rel_change_max

##### Logged flags

- `theta_corr_pass`
- `delta_sd_theta_pass`

Each flag is TRUE or FALSE when eligible, and NA otherwise.

##### Interpretation

The latent scale is no longer meaningfully reshaping or expanding as new
data arrive.

------------------------------------------------------------------------

#### 9.4 Rank Stability Gate (Lagged)

This gate ensures that the **implied ranking itself has stabilized**,
not just the numeric latent scale.

##### Eligibility

The rank stability gate is eligible at refit *t* if:

    t > stability_lag

Otherwise, rank stability metrics are recorded as `NA`.

##### Definition

Let `rank(t)` be the ranking induced by the EAP estimates at refit *t*.

Compute the lagged Spearman rank correlation:

$$\rho_{\text{rank}}(t) = \operatorname{Spearman}\left( \text{rank}(t),\,\text{rank}(t - L) \right)$$

Implementation notes (normative for reproducibility):

- Induced ranks must be computed with deterministic tie handling:
  - `rank(theta_eap, ties.method="average")`

##### Condition

    rho_rank(t) ≥ rank_spearman_min

##### Logged flag

- `rho_rank_pass` (TRUE/FALSE when eligible; NA otherwise)

##### Interpretation

When this gate passes, remaining posterior movement no longer alters the
global ordering in a meaningful way.

------------------------------------------------------------------------

#### 9.5 Stopping Configuration Summary (Defaults)

| Parameter                 | Meaning                                   | Default |
|---------------------------|-------------------------------------------|---------|
| `eap_reliability_min`     | Minimum EAP reliability required          | `0.90`  |
| `stability_lag`           | Lag (in refits) for stability comparisons | `2`     |
| `theta_corr_min`          | Minimum lagged theta correlation          | `0.95`  |
| `theta_sd_rel_change_max` | Maximum relative change in theta SD       | `0.10`  |
| `rank_spearman_min`       | Minimum lagged rank correlation           | `0.95`  |

All stopping parameters are fully configurable. Stopping occurs
immediately when all applicable gates pass at an eligible refit.

------------------------------------------------------------------------

### 10. Outputs and Logs

The primary adaptive outputs are the `step_log`, `round_log`, and
`item_log`. Any additional summary tables are derived views of these
logs and must not introduce new computed quantities or alter recorded
values. All stopping decisions must be reproducible directly from the
logged fields.

#### 10.1 Per-Step Log (`step_log`)

One row per adaptive **step** (one attempted pair selection). This log
supports audit of selection behavior, feasibility, and fallbacks.

At minimum, each `step_log` row must include:

##### Step identity and outcome

- `step_id`
- `pair_id` (monotone index of committed comparisons; `NA` if no pair
  committed)
- selected items: `i`, `j` (unordered endpoints; `NA` if none selected)
- ordered presentation: `A`, `B` (ordered; `NA` if none selected)
- outcome `Y` (1 = A wins, 0 = B wins; `NA` if no committed result)

##### Selection routing and health

- `is_explore_step` (TRUE/FALSE)
- `explore_mode` (`local`, `nonlocal`, or `NA`)
- `explore_reason` (e.g., `probabilistic`, `coverage_quota_override`,
  `NA`)
- `candidate_starved` (TRUE/FALSE)
- `fallback_used` (one of: `base`, `expand_locality`,
  `uncertainty_pool`, `dup_relax`, `global_safe`; `base` if none)
- `fallback_path` (e.g., `base>expand_locality>dup_relax`; `base` if
  none)
- `starvation_reason` (best-effort; `NA` if not starved)
- `explore_rate_used` (numeric; effective exploration rate applied at
  this step)
- `local_priority_mode` (`standard`, `near_tie`, `boundary`, or `NA`)
- `long_gate_pass` (TRUE/FALSE/NA)
- `long_gate_reason` (e.g., `posterior_extreme`,
  `posterior_unavailable`, `NA`)
- `star_override_used` (TRUE/FALSE) (should be FALSE (not NA) when a
  pair is committed and no override was needed, and NA only when no pair
  was selected)
- `star_override_reason` (e.g., `near_tie_override`, `budget_exhausted`,
  `deg_extreme`, `NA`)

##### Candidate counts (per step; aligned to canonical pipeline §5.2.1)

- `n_candidates_generated`
- `n_candidates_after_hard_filters`
- `n_candidates_after_duplicates`
- `n_candidates_after_star_caps`
- `n_candidates_scored` (equals `n_candidates_after_star_caps` when
  degree regularization is off)

##### Key endpoint diagnostics

- `deg_i`, `deg_j`
- `recent_deg_i`, `recent_deg_j`
- `mu_i`, `mu_j`
- `sigma_i`, `sigma_j`
- `p_ij` and `U0_ij` for the selected pair (if committed; else `NA`)

##### Round scheduling and strata fields

- `round_id` (current round index; increments when a new round begins)

- `round_stage` ∈ {`anchor_link`, `long_link`, `mid_link`, `local_link`}

- `pair_type` (alias of `round_stage`; included for convenience)

- `used_in_round_i`, `used_in_round_j` (0/1/2 usage counts at time of
  selection; `NA` if no selection)

- `is_anchor_i`, `is_anchor_j` (TRUE/FALSE/NA)

- `stratum_i`, `stratum_j` (integer stratum ids; `NA` if no selection)

- `dist_stratum` (\|stratum_i − stratum_j\|; `NA` if no selection)

- `stage_committed_so_far` (committed pairs already produced within this
  round and stage, prior to this step; `NA` if not applicable)

- `stage_quota` (target committed pairs for this round-stage; `NA` if
  not applicable)

##### Star-cap diagnostics (per step)

- `star_cap_rejects`
- `star_cap_reject_items` (unique items triggering star-cap rejections)

This schema is intentionally redundant to support debugging, regression
tests, and post-hoc analysis.

------------------------------------------------------------------------

#### 10.2 Per-Refit Log (`round_log`)

Despite its name, `round_log` records one row per Bayesian refit (not
per Pollitt-style round). The field `round_id_at_refit` links each refit
to the most recent completed scheduling round.

This log is the primary “stop audit trail”: every stopping decision must
be reproducible from these fields.

At minimum, each `round_log` row must include:

##### Run scale / sampling state

- `refit_id` (1, 2, …; one row per Bayesian refit)
- `round_id_at_refit` (the Pollitt-style round index at the time this
  refit was run)
- `step_id_at_refit`
- `model_variant`
- `n_items`
- `total_pairs_done` (total committed comparisons at refit)
- `new_pairs_since_last_refit`
- `n_unique_pairs_seen`

##### Candidate/selection health (stepwise)

- `proposed_pairs_mode` (typical `n_candidates_scored` per step since
  last refit; e.g., median)
- `starve_rate_since_last_refit` (fraction of steps with
  `candidate_starved=TRUE`)
- `fallback_rate_since_last_refit` (fraction of steps with
  `fallback_used != base`, where `base` denotes no fallback beyond the
  stage’s default candidate rule).
- `fallback_used_mode` (most frequent fallback stage since last refit)
- `starvation_reason_mode` (most frequent starvation reason among
  starved steps)

##### Global identifiability and quota adaptation

- `global_identified` (TRUE/FALSE)

- `global_identified_reliability_min`

- `global_identified_rank_corr_min`

- `long_quota_raw`

- `long_quota_effective`

- `long_quota_removed`

- `realloc_to_mid`

- `realloc_to_local`

These quota fields refer to the most recently completed Pollitt-style
round prior to the refit (i.e., the round indexed by
`round_id_at_refit`).

##### Imbalance / coverage diagnostics

- `mean_degree`, `min_degree`
- `pos_balance_sd` (SD of per-item position balance; near 0 is ideal)

##### Model-parameter monitoring (posterior percentiles)

For any global model parameters included in the fitted model,
`round_log` must record posterior mean and a fixed set of percentiles.

- **Lapse rate** `epsilon` (Models B and D; otherwise `NA`):

  - `epsilon_mean`
  - `epsilon_p2.5`, `epsilon_p5`, `epsilon_p50`, `epsilon_p95`,
    `epsilon_p97.5`

- **Position bias** `b` (Models C and D; otherwise `NA`):

  - `b_mean`
  - `b_p2.5`, `b_p5`, `b_p50`, `b_p95`, `b_p97.5`

##### TrueSkill monitoring (audit-only)

- TrueSkill summary at refit:

  - `ts_sigma_mean`
  - `ts_sigma_max`
  - `ts_degree_sigma_corr`

- Alignment with BTL estimates:

  - `ts_btl_theta_corr`
  - `ts_btl_rank_spearman`

##### Near-tie and credible-interval width diagnostics (BTL posterior; audit + interpretability)

These diagnostics do not affect stopping directly unless explicitly
configured, but must be logged to support the “residual uncertainty
dominated by near-ties” interpretation.

Compute per-item 95% credible-interval widths:

- `ci95_theta_width_i = theta_p97.5[i] - theta_p2.5[i]`

Log summary widths:

- `ci95_theta_width_mean` = mean of `ci95_theta_width_i`
- `ci95_theta_width_median` = median of `ci95_theta_width_i`
- `ci95_theta_width_p90` = 90th percentile of `ci95_theta_width_i`
- `ci95_theta_width_max` = max of `ci95_theta_width_i`

Compute near-tie diagnostics based on posterior win-probabilities for
adjacent ranks:

- Let items be ordered by EAP rank at refit `t`: `r1, r2, ..., rN`.
- For each adjacent pair `(r_k, r_{k+1})`, compute:
  - `p_adj_k = Pr(theta_{r_k} > theta_{r_{k+1}} | posterior)`
  - `near_tie_k = (p_adj_k ∈ [near_tie_p_low, near_tie_p_high])`

Log:

- `near_tie_adj_frac` = mean of `near_tie_k` over k=1..N-1
- `near_tie_adj_count` = sum of `near_tie_k`
- `p_adj_median` = median of `p_adj_k`

Defaults:

    near_tie_p_low = 0.40
    near_tie_p_high = 0.60

Implementation note (normative): estimate `p_adj_k` using posterior
draws as: - `p_adj_k = mean( I(theta_draw[r_k] > theta_draw[r_{k+1}]) )`
computed over all retained post-warmup draws (across all chains).

##### Additional global efficiency metrics (report-only; not used for stopping)

Posterior concentration / covariance (θ): - `cov_trace_theta` (trace of
posterior covariance of θ; lower is better concentration) -
`cov_logdet_diag_theta` (log det of diag(cov(θ)); stable proxy for
overall uncertainty) - `post_sd_theta_p10`, `post_sd_theta_p50`,
`post_sd_theta_p90` (distribution of per-item posterior SDs)

Top-k decision uncertainty: - `top20_boundary_entropy_mean` (uncertainty
mass around the top-20 cutoff; lower is better) -
`top20_boundary_entropy_p90`

Local separation uncertainty: - `nn_diff_sd_mean` (posterior SD of
adjacent-rank θ differences; lower indicates cleaner local separation) -
`nn_diff_sd_p90`

##### Stopping gates and core posterior summaries

- `diagnostics_pass`, `divergences`, `max_rhat`, `min_ess_bulk`,
  `ess_bulk_required`
- `reliability_EAP`
- `theta_sd_eap`
- `rho_theta` and `delta_sd_theta` (lagged theta stability; §9.3)
- `rho_rank` and `rho_rank_pass` (lagged rank stability; §9.4)

Lagged metrics are computed only when eligible; otherwise `NA`.

##### Stop decision

- `stop_decision` (TRUE/FALSE)
- `stop_reason` (if stopped)

------------------------------------------------------------------------

#### 10.3 Item-Level Log (`item_log`)

Item-level posterior summaries are recorded at each refit.

The item log is stored internally as a list of per-refit tables, indexed
by refit number. Each table contains item-level posterior summaries for
that refit.

##### Contents (per refit)

At minimum, each item-level table includes:

- `refit_id`
- `item_id`
- posterior mean $\widehat{\theta}$
- posterior percentiles (e.g., p2.5 / p50 / p97.5)
- induced rank
- item degree
- position exposure counts

##### Access and summaries

- By default, summary functions return the most recent refit’s item
  table.
- Users may request a specific refit by numeric index.
- Optionally, all refits may be stacked for longitudinal analysis.

##### Disk writing

Writing item-level logs to disk is optional. When enabled, one file per
refit is written. Item-level logging itself is always performed
in-memory, regardless of disk output settings.

------------------------------------------------------------------------

### 11. Interpretation Guarantees

When the algorithm stops:

- Ranking is globally stable
- Residual uncertainty is dominated by near‑ties
- MCMC error is negligible
- Sampling imbalance is controlled and measurable
- ESS thresholds scale with problem size, ensuring that numerical
  precision of posterior summaries is comparable across different values
  of `N`.

------------------------------------------------------------------------

### 12. Design Philosophy

- TrueSkill model to guide adaptive pairing; Bayesian BTL model to
  determine stopping
- One utility
- Explicit invariants
- Adaptive but auditable
- Defaults scale with N
- Diagnostics first, always
- **User-facing documentation:** All exported functions and outputs must
  include detailed Roxygen documentation describing argument options
  (including model variants and stopping parameters), defaults, and the
  meaning/interpretation of key output columns. A user should be able to
  understand what each logged value means without inspecting
  implementation code.
