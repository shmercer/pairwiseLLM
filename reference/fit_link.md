# Fit a linker using prepared explicit evidence

Fit a linker using prepared explicit evidence

## Usage

``` r
fit_link(input, previous = NULL)
```

## Arguments

- input:

  A validated object from
  [`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md).

- previous:

  Optional common linking result for numerical warm starting. Supply the
  complete cumulative cross-set evidence in `input`. The old prefix,
  identities, Phase A inputs, judge surface, and offset prior must be
  unchanged. Only the previous numerical mode is passed to an engine,
  never its posterior as a new prior. Invalid previous fits supply no
  warm start.

## Value

A `pairwiseLLM_link_result` containing common-scale item summaries,
offset summaries, joint uncertainty, prediction data, diagnostics,
provenance, and continuation inputs. Unavailable uncertainty is typed
missing, never implicitly zero. Numerical failures remain invalid
method-specific results.

## Details

E1 (`fixed_shape_offset`) uses adaptive one-dimensional quadrature. E2
and E3 raise `pairwiseLLM_link_not_implemented`; there is no default or
fallback. Result schema version 1 uses `theta_H = H_H u_H` and
`theta_S = delta + H_S u_S`, 95 percent interval endpoints, descending
ranks with average ties, and free coordinates delta, hub shape, then
spoke shape. E1 has only the delta free coordinate. Prediction and
continuation state contain serializable data, with functions resolved by
estimator ID/version.

E1 holds separately centered Phase A EAP shapes fixed and updates only
delta under the configured Normal prior and the supplied cross-set
likelihood. Reported means, SDs, and equal-tailed 95 percent intervals
come from adaptive Gauss-Kronrod quadrature, with CDF integration and
root finding for interval endpoints. Both infinite tails are integrated
using rational coordinate maps; there is no finite-domain truncation or
Laplace approximation. Monotone likelihood bounds guard against missed
distant mass. With zero edges, or epsilon equal to one, summaries use
the exact Normal prior. Positive-budget epsilon-one fits report
`unidentified`.

E1 uncertainty is conditional on fixed shapes: hub variance is truly
zero, every spoke SD equals delta SD, and all spoke uncertainty is
perfectly correlated. It does not propagate Phase A estimation
uncertainty. E1 is useful with strong Phase A estimates or when
computation must be inexpensive. Numerical initial values are ignored;
continuation recomputes the posterior from the original prior and
cumulative evidence, deterministically.

## Result fields

`items` contains `set_id`, `item_id`, `global_item_id`,
`theta_link_mean`, `theta_link_sd`, `theta_link_lower`,
`theta_link_upper`, and `rank_link`. `offset` contains `delta_mean`,
`delta_sd`, `delta_lower`, `delta_upper`, and `identification`
(`prior_only`, `cross_set`, `unidentified`, or `failed`). At zero cross
edges a valid result retains the configured Normal offset prior and
reports `prior_only`. E1 retains fixed shapes; E2 retains independent
full Gaussian Phase A bridges; E3 fits only the centered Phase A shape
posterior.

`uncertainty` contains the named free-coordinate `covariance` (or
`NULL`), `basis`, and `item_transform`. For transform T and covariance
V, item-scale covariance is `T %*% V %*% t(T)`. E1's item transform has
only a delta column; its fixed shapes supply the deterministic part of
item means.

`diagnostics` records `fit_attempted`, `fit_valid`, `convergence_code`,
`finite_objective`, `finite_gradient`, `hessian_pd`, `covariance_valid`,
`covariance_jitter`, `warning_code`, `failure_code`,
`uncertainty_scope`, `sampler`, `elapsed_seconds`, `cpu_seconds`,
`peak_memory_bytes`, and `n_parameters`. Undefined fields use typed
missing values. Invalid fits carry a failure code and cannot be used for
prediction. E1 adds optional `quadrature` diagnostics: method, domain,
coordinate, effective controls, integration partitions, subdivision
count, log normalizer, estimated mass/ moment errors, CDF error and root
brackets when computed, summary method, and status. Partial diagnostics
remain available after numerical failure. Quadrature errors are
numerical estimates, not posterior SDs. Valid E1 scope is
`offset_only_conditional_on_fixed_shapes`; valid E2/E3 scope is
`joint_shapes_and_offset`. A true zero conditional variance is allowed.

`provenance` retains package/source versions, all input hashes and
counts, Phase A sources, and the judge source. `prediction` binds
serializable estimator data to estimator ID/version and input hash.
`continuation` retains the original normalized `input` and optional
free-coordinate numerical `mode`. Results can be round-tripped with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and
[`readRDS()`](https://rdrr.io/r/base/readRDS.html).

## See also

[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`predict_link()`](https://shmercer.github.io/pairwiseLLM/reference/predict_link.md)
