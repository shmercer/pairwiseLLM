# Adaptive Pairing and Ranking in pairwiseLLM

## Overview

The `pairwiseLLM` package supports ranking writing samples (or other
items) using pairwise comparisons. When the number of items is large or
judgments are expensive, choosing which pairs to compare becomes as
important as how the comparisons are modeled.

Adaptive pairing is designed to allocate comparisons more efficiently by
focusing effort where it reduces uncertainty the most.

At a high level:

- Pair selection is adaptive and uncertainty-aware.
- Fewer total comparisons are needed to reach stable rankings.
- Remaining uncertainty is concentrated in genuinely ambiguous cases
  (near-ties).
- Final rankings are accompanied by principled Bayesian uncertainty
  estimates.

This vignette introduces the adaptive pairing and ranking workflow and
explains when and why it is useful.

------------------------------------------------------------------------

## Why adaptive pairing?

A simple baseline approach to pairwise ranking is random pairing: select
pairs uniformly at random and fit a model once enough data have been
collected. Random pairing is straightforward to understand and often
yields slightly higher global reliability metrics.

However, random pairing is inefficient:

- Many comparisons are redundant or uninformative.
- Obvious mismatches consume judgment budget.
- Uncertainty remains unnecessarily large in decision-relevant regions
  of the ranking.

Adaptive pairing addresses this by allocating comparisons where they
matter most.

### Reliability vs. precision

In practice, adaptive pairing often produces:

- Slightly lower overall Bayesian EAP reliability compared to random
  pairing, because comparisons are intentionally concentrated rather
  than uniformly distributed.
- Substantially tighter credible (confidence) intervals around latent
  quality scores and rankings, especially among closely ranked items.

This tradeoff is intentional. Adaptive pairing prioritizes precision of
estimates and rankings over uniform coverage of the latent scale.

For applications such as writing quality evaluation, where distinctions
between nearby samples matter more than broad global separation, this
tradeoff is often desirable.

------------------------------------------------------------------------

## Conceptual model

Adaptive ranking in `pairwiseLLM` separates pair selection from
inference:

- A fast model (TrueSkill) guides which pair to compare next after each
  new pair is evaluated.
- A Bayesian Bradley–Terry–Luce (BTL) model is used intermittently to
  estimate latent quality, quantify uncertainty, and determine stopping.

This separation allows the system to adapt quickly while still producing
fully Bayesian final results.

------------------------------------------------------------------------

## The adaptive workflow

An adaptive run proceeds through repeated cycles:

1.  **Select a pair** A candidate pair is chosen based on uncertainty,
    coverage needs, and structural constraints.

2.  **Obtain a judgment** The pair is evaluated (typically by an LLM),
    producing a binary outcome.

3.  **Online update** The online state is updated immediately.

4.  **Periodic Bayesian refit** After enough new comparisons, all
    accumulated data are refit using Bayesian BTL to produce posterior
    estimates, diagnostics, and stopping metrics.

This cycle repeats until stopping criteria are met.

------------------------------------------------------------------------

## Early vs. late stages of adaptation

Adaptive behavior intentionally changes over the course of a run.

### Early stage: global scale identification

Early in the run, the algorithm focuses on establishing a well-connected
comparison graph and identifying the global latent scale. This involves:

- comparisons that link distant parts of the ranking,
- anchor items that connect many samples,
- exploratory pairing to avoid isolated samples.

The goal is to ensure that all samples are meaningfully connected before
refinements are made.

### Late stage: local refinement

Once the global scale is reliably identified, adaptive behavior shifts:

- long-range comparisons are reduced,
- more comparisons are allocated to similar-quality samples,
- near-ties receive focused attention.

This concentrates effort where remaining uncertainty affects ranking
decisions.

------------------------------------------------------------------------

## Global identifiability and stopping

A key feature of the adaptive workflow is an explicit notion of global
identifiability.

At each Bayesian refit, diagnostics are used to assess whether:

- latent quality estimates are reliable, and
- rankings implied by TrueSkill and Bayesian models agree.

Once global identifiability is achieved, it gates later-stage behavior
and enables more aggressive local refinement.

Stopping decisions are made only when:

- Bayesian diagnostics are satisfactory,
- posterior reliability is high,
- quality scores and rankings are stable across refits.

This ensures that adaptive efficiency does not come at the cost of
statistical defensibility.

------------------------------------------------------------------------

## How the adaptive algorithm works (schematic overview)

Adaptive pairing in `pairwiseLLM` combines fast online selection with
slower Bayesian inference. The key idea is that pair selection and
inference operate on different time scales.

At each step, only one pair is selected and evaluated. Bayesian refits
occur intermittently and are used to guide later behavior and stopping
decisions.

### High-level flow

``` text
 ┌──────────────────────┐
 │  Initialize run      │
 │  (warm start)        │
 └─────────┬────────────┘
           │
           ▼
 ┌──────────────────────┐
 │  Adaptive pairing    │
 │  (step-by-step)      │
 │                      │
 │  • select pair       │
 │  • query judge       │
 │  • update online     │
 │    state             │
 └─────────┬────────────┘
           │  (after enough pairs)
           ▼
 ┌──────────────────────┐
 │  Bayesian BTL refit  │
 │                      │
 │  • posterior θ       │
 │  • uncertainty       │
 │  • diagnostics       │
 │  • stability checks  │
 └─────────┬────────────┘
           │
           ▼
 ┌──────────────────────┐
 │  Adapt behavior      │
 │  & check stopping    │
 │                      │
 │  • taper long links  │
 │  • focus near-ties   │
 │  • reduce exploration│
 └─────────┬────────────┘
           │
           ├── if stop = FALSE ──► back to adaptive pairing
           │
           └── if stop = TRUE ──► return results
```

------------------------------------------------------------------------

### Early vs. late behavior

- **Early phase**

  - Emphasizes coverage and global scale identification
  - Uses anchor, long-range, and mid-range comparisons
  - Exploration remains relatively high

- **Late phase**

  - Triggered once the global scale is statistically identified
  - Long-range comparisons are reduced
  - Comparisons focus on near-ties
  - Exploration is tapered

This transition is data-driven, not based on a fixed number of
iterations.

------------------------------------------------------------------------

## Worked example: adaptive ranking of writing samples

This example demonstrates a complete adaptive ranking workflow using
example writing samples included in the package.

We show how to: - load example data, - select a writing trait to
evaluate, - define a prompt template, - run adaptive ranking with a live
LLM judge, - and extract logs for further analysis.

------------------------------------------------------------------------

### Run adaptive ranking

Below is an example of the full adaptive workflow using
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md).

Key configuration choices in this example:

- **Model**: `gpt-5.1`
- **Service tier**: `"flex"` (priority routing)
- **Custom stopping thresholds**: slightly relaxed reliability, stricter
  rank stability

``` r
library(pairwiseLLM)

# Example data shipped with the package
data("example_writing_samples")

# Choose a built-in trait (or define a custom trait$name and trait$description)
trait <- "overall_quality"

# Prompt template (character string with required placeholders)
tmpl <- set_prompt_template()  # or set_prompt_template(template = "...custom...")

# Use custom stopping criteria
btl_config <- list(
  # --- default values ---
  eap_reliability_min = 0.90,        # Bayesian EAP reliability threshold
  stability_lag = 2L,                # number of refits to use for stability checks
  theta_corr_min = 0.95,             # correlation between θ across refits (stability)
  theta_sd_rel_change_max = 0.10,    # relative change in posterior SD across refits
  rank_spearman_min = 0.95,          # rank stability threshold across refits
)

set.seed(123)

res <- adaptive_rank(
  data = example_writing_samples,
  id_col = "sample_id",
  text_col = "text",

  backend = "openai",
  model = "gpt-5.1",
  trait = trait,
  prompt_template = tmpl,

  # Service tier / routing lives inside judge_args
  judge_args = list(service_tier = "flex"),

  # Pass btl_config for stopping rules
  btl_config = btl_config,

  # Run for up to N steps; the run may stop early if stopping rules pass
  n_steps = 5000L,
)
```

For most users, this single function call is sufficient to run an
adaptive study end-to-end.

------------------------------------------------------------------------

### Inspecting results

#### Ranked items with uncertainty

``` r
item_summary <- res$items     # summarize_items(...)
head(item_summary)
```

Typical columns include:

- posterior mean latent quality,
- credible interval bounds,
- induced rank,
- total number of comparisons per item.

These summaries reflect the final Bayesian refit.

------------------------------------------------------------------------

#### Step-level logs (pair selection audit trail)

``` r
step_log <- adaptive_step_log(res$state)
head(step_log)
```

Each row corresponds to one attempted adaptive step and records:

- which pair was selected,
- which stage it came from (anchor / long / mid / local),
- whether exploration was used,
- whether fallbacks or overrides occurred.

This log allows detailed analysis of adaptive behavior.

------------------------------------------------------------------------

#### Pair result history

``` r
# Pair result history (committed outcomes)
pair_history <- subset(step_log, status == "ok", select = c(
  step_id, timestamp, pair_id,
  i, j, A, B, Y,
  round_id, round_stage, pair_type
))

head(pair_history)
```

This table contains the committed comparison outcomes:

- ordered pairs,
- binary results,
- timing and refit indices.

It can be reused for:

- refitting alternative models,
- agreement analyses,
- simulation studies.

------------------------------------------------------------------------

#### Refit-level summaries and stopping diagnostics

``` r
refit_log <- res$refits    # summarize_refits(...)
refit_log
```

The refit log records:

- Bayesian diagnostics,
- reliability and stability metrics,
- whether and why stopping occurred.

This provides a transparent justification for termination.

------------------------------------------------------------------------

### Downstream analyses

Because all adaptive decisions are logged, you can:

- examine how uncertainty evolves over time,
- compare adaptive vs. random pairing efficiency,
- refit models with alternative assumptions,
- visualize how near-ties concentrate late-stage effort.

------------------------------------------------------------------------

## When should you use adaptive ranking?

Adaptive pairing is particularly useful when:

- the number of items is large,
- judgments are expensive or slow,
- fine distinctions between nearby items matter,
- uncertainty estimates are as important as point rankings.

For very small item sets or exploratory analyses, simpler random pairing
designs may still be appropriate.

------------------------------------------------------------------------

## Technical details

This vignette focuses on how to use adaptive pairing.

For a complete technical specification of the adaptive algorithm,
including: - pair types and quotas, - fallback rules, - Bayesian models
and diagnostics, - logging guarantees,

see the companion vignette:

- [Bayesian BTL Adaptive Pairing
  Design](https://shmercer.github.io/pairwiseLLM/articles/bayesian-btl-adaptive-pairing-design.md)

------------------------------------------------------------------------

## Summary

Adaptive pairing in `pairwiseLLM` reallocates comparisons toward the
most informative parts of the ranking, reducing uncertainty where it
matters most. While overall reliability metrics may be slightly lower
than with random pairing, adaptive designs typically produce tighter
credible intervals and more decision-relevant rankings, making them well
suited for writing quality evaluation and similar tasks.
