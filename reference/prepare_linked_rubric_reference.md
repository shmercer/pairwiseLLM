# Save a standalone Bayesian ranking as a reusable rubric reference

Prepare a completed reference ranking before scoring any new samples.
This supports reference rankings fitted from pooled historical
comparisons as well as a single collection of comparisons. The original
scores and comparisons are saved together so later linking can check
that it uses the same reference.

## Usage

``` r
prepare_linked_rubric_reference(
  cj,
  evidence,
  set_id,
  items = NULL,
  trait = NULL,
  provenance = list()
)
```

## Arguments

- cj:

  A completed
  [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)
  result. With several refits, the last refit is used. It must record
  evidence identity at fit time. Older results must be refitted from
  their original comparisons first; they can still be used for ordinary
  same-set rubric calibration.

- evidence:

  The exact comparison rows used for that refit, in the same
  results-table format supplied to
  [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md).
  For a subset fit, supply only that subset. See
  [`build_btl_results_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_btl_results_data.md).

- set_id:

  A single reference-set identifier, as in
  [`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md).

- items:

  Optional data frame with `item_id` and `global_item_id`, mapping every
  fitted item to a unique ID across sets. When omitted, fitted item IDs
  are also used as global IDs.

- trait:

  The trait being ranked, such as `"organization"`. Required when the
  completed result does not already record it.

- provenance:

  Optional named list of serializable study/source metadata, such as
  `source_commit`. Stored separately from computed identity fields.

## Value

A serializable `pairwiseLLM_linked_rubric_reference`. Supply it to
[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md)
with `calibration_design = "linked_anchors"`. `items` holds global IDs,
source IDs, frozen scores and SDs. For linking, `hub` supplies the
set/item mapping; `points`, `posterior_draws`, and `evidence` supply the
E1, E2, and E3 hub inputs, respectively. Attach `source` to each input
to carry the same computed `reference_hash`. `judge` contains the shared
bias/lapse settings from the accepted fit. Diagnostics and reliability
are retained; failed diagnostics warn as in ordinary calibration.

## Details

The supplied comparisons must match the evidence recorded when the fit
was made. Compatible item names alone are insufficient. Changed
comparisons require a new fit and a new reference. Save the result with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and restore it with
[`readRDS()`](https://rdrr.io/r/base/readRDS.html); no live sampler is
needed for later prediction.

This constructor supports standalone fits with shared judge parameters.
Import-ready adaptive Phase A artifacts continue to work directly with
[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md).
Neither route changes the original reference scores to match the
distribution of a new set.

## See also

[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md)

Other rubric calibration:
[`evaluate_rubric_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/evaluate_rubric_predictions.md),
[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md),
[`predict.pairwiseLLM_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_rubric_calibration.md)
