# Run and resume explicit-evidence linking sessions

Sessions retain common E1–E3 results under `linking$estimator`,
including estimator ID/version, per-spoke continuation, diagnostics and
exact provenance. Pair selection and stopping remain external. No
estimator is selected by default.

## Usage

``` r
start_link_session(input, status = "active")

resume_link_session(state, input, status = NULL)
```

## Arguments

- input:

  A
  [`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md)
  object, or (for `start_link_session`) a list of such objects with
  distinct spokes and identical hub evidence/judge.

- status:

  Explicit reporting status: `active`, `probe`, or `frozen`. This labels
  controller status only: all observations in `input$cross` remain
  active estimation evidence. Held-out probe outcomes must not enter
  that table.

- state:

  A linking session from `start_link_session()` or
  [`load_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md).

## Value

A `pairwiseLLM_link_session` with exact data-only results and a stage
log.

## Details

Resume requires the same estimator, version, item order, Phase A, judge,
numerical configuration and unchanged old cross-evidence prefix. New
rows may only be appended. Passing the identical input is an exact no-op
(including MCMC); extended evidence refits using the saved numerical
mode, never the previous posterior as a new prior. A frozen spoke cannot
append evidence until the caller explicitly sets `status = "active"`.

With multiple spokes, each estimator fits its own hub/spoke pair. E2/E3
hub posteriors can differ across spokes. Item summaries retain
`link_spoke_id` and do not average or silently select one hub posterior.
Ranks are per fit.

## See also

[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
`start_link_session()`,
[`save_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md)

Other linking:
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
[`predict_link()`](https://shmercer.github.io/pairwiseLLM/reference/predict_link.md),
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`save_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md),
[`summary.pairwiseLLM_link_result()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_link_result.md)
