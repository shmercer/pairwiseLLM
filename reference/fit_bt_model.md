# Fit a Bradley-Terry model with optional frequentist engines

This function fits a Bradley–Terry paired-comparison model to data
prepared by
[`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md).
It supports four modeling engines:

- sirt: [`btm`](https://rdrr.io/pkg/sirt/man/btm.html) — the default
  engine, which produces ability estimates, standard errors, and MLE
  reliability.

- BradleyTerry2: [`BTm`](https://rdrr.io/pkg/BradleyTerry2/man/BTm.html)
  — used as a fallback if sirt is unavailable or fails; computes ability
  estimates and standard errors, but not reliability.

- brglm2: explicit Firth mean bias reduction for random or nonadaptive
  schedules, with centered covariance and SSR.

- `alpha`: explicit alpha adjustment motivated by adaptive schedules,
  using base R with centered covariance and SSR.

## Usage

``` r
fit_bt_model(
  bt_data,
  engine = c("auto", "sirt", "BradleyTerry2", "brglm2", "alpha"),
  verbose = TRUE,
  ...,
  sirt_eps = NULL,
  alpha = NULL
)
```

## Arguments

- bt_data:

  A data frame or tibble with exactly three columns: two character ID
  columns and one numeric `result` column equal to 0 or 1. Usually
  produced by
  [`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md).

- engine:

  Character string specifying the modeling engine. One of: `"auto"`
  (default), `"sirt"`, `"BradleyTerry2"`, `"brglm2"`, or `"alpha"`.
  Automatic selection never chooses Firth or alpha.

- verbose:

  Logical. If `TRUE` (default), show engine output (iterations,
  warnings). If `FALSE`, suppress noisy output to keep examples and
  reports clean.

- ...:

  Additional arguments passed through to
  [`sirt::btm()`](https://rdrr.io/pkg/sirt/man/btm.html) or
  [`BradleyTerry2::BTm()`](https://rdrr.io/pkg/BradleyTerry2/man/BTm.html).
  For `brglm2`, only a named `control` list is accepted, with numerical
  settings `epsilon` (default `1e-10`), `maxit` (200), `slowit` (1),
  `max_step_factor` (12), and `trace` (FALSE). `verbose = FALSE`
  disables tracing; numerical warnings are retained. The
  mean-bias-reduction method cannot be changed through controls. For
  `alpha`, only a named `control` list is accepted: `epsilon` (default
  `1e-12`), `maxit` (200), `gradient_tol` (`1e-7`), `step_tol` (`1e-7`),
  `min_rcond` (`1e-12`), and `trace` (FALSE). See the alpha section
  below.

- sirt_eps:

  Optional finite, nonnegative epsilon adjustment for sirt, supplied by
  exact name. `NULL` preserves the engine default or legacy `eps` in
  `...`. Supplying both forms raises an error. This argument is valid
  only with `engine = "sirt"` or `"auto"`; on automatic fallback it
  remains recorded as requested but is not applied to BradleyTerry2.

- alpha:

  Explicit finite nonnegative numeric scalar, supplied by exact name,
  required only for `engine = "alpha"`. There is no default penalty and
  no tuning from outcomes. Values 0.30 and 0.50 are supported alongside
  other nonnegative values. Zero requests ordinary unpenalized
  estimation and requires a strongly connected directed win graph.
  `NULL` is only accepted for other engines.

## Value

A list with the following elements:

- engine:

  The engine actually used ("sirt", "BradleyTerry2", "brglm2", or
  "alpha").

- fit:

  The fitted model object.

- theta:

  A tibble with columns:

  - `ID`: object identifier

  - `theta`: estimated ability parameter

  - `se`: standard error of `theta`

- reliability:

  Raw MLE reliability for sirt or calculated SSR for Firth/alpha. `NA`
  for BradleyTerry2 models or a zero-variance Firth/alpha fit.

- ssr:

  For sirt, the
  [`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md)
  decomposition plus `engine_reliability`, `agrees`,
  `absolute_difference`, and `tolerance`. For BradleyTerry2, `ssr` and
  `engine_reliability` are `NA`, `valid` is `FALSE`, `agrees` is `NA`,
  and `status` is `"unavailable_se_convention"`. For Firth/alpha, the
  helper decomposition, or `valid = FALSE` and
  `status = "zero_score_variance"` when undefined.

- provenance:

  A list recording `engine`, `requested_engine`, loaded `engine_version`
  and `package_version`, `supplied_arguments`, `requested_sirt_eps`,
  `effective_settings`, `adjustment`, `identification`, `convergence`
  (status, converged, iterations), `theta_finite`, `se_finite`,
  `reliability_valid`, `reliability_status`, and `fallback_reason`
  (`NULL` unless automatic fallback occurred). Identification records
  sirt centering or BradleyTerry2's contrasts, reference category and
  player levels. Save the full object to retain these settings; the
  legacy summary tibble is unchanged. Firth provenance also records the
  coordinate transformation and covariance convention. Alpha adds
  `engine_package = "stats"`, the explicit penalty, solver, parameter
  ordering, convergence code/message and uncertainty scope.

- vcov:

  Firth/alpha: centered item covariance matrix, with row/column labels
  in the same order as `theta$ID`.

- comparisons:

  Firth/alpha: original item pairs for default prediction.

- alpha:

  Alpha engine only: the requested penalty strength.

- diagnostics:

  Alpha engine only: objective components, item scores,
  reduced-coordinate gradient, penalized Hessian and unpenalized
  information, matrix checks, Newton correction, optimizer status, and
  numerical warnings.

## Details

When `engine = "auto"` (the default), the function attempts sirt first
and automatically falls back to BradleyTerry2 on availability or
execution failure. Explicit engine requests never fall back. Invalid
inputs, disconnected graphs, and invalid reliability results raise
errors even in automatic mode. The output format is standardized, so
downstream code can rely on consistent fields.

The input `bt_data` must contain exactly three columns:

1.  object1: character ID for the first item in the pair

2.  object2: character ID for the second item

3.  result: numeric indicator (1 = object1 wins, 0 = object2 wins)

Ability estimates (`theta`) represent latent "writing quality"
parameters on a log-odds scale. Higher values mean stronger relative
performance on the assessed trait. Zero is not a pass mark, and these
estimates are not rubric grades or automatically comparable across
independently fitted sets. Standard errors are included for all modeling
engines. Raw engine MLE reliability is available from sirt; Firth and
alpha fits return independently calculated SSR.

For sirt, `$ssr` independently calculates
`1 - mean(se^2) / stats::var(theta)` from all returned items, using
sample variance. Agreement with `$fit$mle.rel` is required within
`1e-12 * max(1, abs(engine_reliability), abs(ssr))`. Negative SSR is
retained. Nonfinite theta/SEs, negative SEs, zero score variance, and
nonfinite calculations raise errors without dropping items. This
includes sirt fits with missing SEs from fixed theta or extreme scores
when epsilon is zero. See
[`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md)
for the component definitions.

SSR depends on estimated score variance and the SE convention: it is not
an estimator-free measure of recovery. BradleyTerry2 uses engine
contrasts (normally a reference item); this wrapper does not calculate
SSR from its reference-based SEs. Its legacy `$reliability` remains
`NA`.

The sirt default epsilon is resolved from the installed engine (0.3 in
sirt 4.2.133), and the returned epsilon is checked and recorded. Other
estimator defaults are unchanged, including sirt's tie and positional
parameters. `effective_settings` records resolved arguments; for sirt,
`fix.delta_requested` is separated from `returned_parameters` because
sirt 4.2.133 accepts but does not apply `fix.delta`. Other engine
versions are marked unverified for that argument. No fix for the
upstream estimator is applied here.

Connectivity is checked before any engine is called, including ties
removed by `ignore.ties = TRUE`, and BradleyTerry2 subsets/zero weights.
Disconnected data cannot identify global BT scores or SSR. Missing
outcomes, invalid IDs, and self-comparisons raise errors rather than
being dropped. Direct sirt inputs may include ties coded 0.5; the
BradleyTerry2 wrapper requires binary outcomes.

sirt provides iterations but no explicit convergence flag. Early
termination is recorded as `stopping_criterion_met`; reaching `maxiter`
is recorded as `iteration_limit_reached` with `converged = NA`, not as
proven convergence or nonconvergence. BradleyTerry2's reported
convergence is preserved. Reliability validity describes the arithmetic,
not proof of convergence.

Install an optional engine before fitting, for example with
`install.packages("sirt")`. Pairwise data preparation does not need that
engine. See the [offline
walkthrough](https://shmercer.github.io/pairwiseLLM/articles/getting-started.html)
for fitting and interpreting bundled synthetic comparisons.

Firth fits use binomial-logit
[`brglm2::brglmFit`](https://rdrr.io/pkg/brglm2/man/brglmFit.html) with
`type = "AS_mean"`, equivalent to adding half the log determinant of
expected information to the log likelihood. This is a genuine Firth
estimator, not sirt epsilon adjustment. It is an explicit option for
random/nonadaptive schedules; it is not recommended here as the primary
adaptive-schedule correction. No schedule type is inferred from
outcomes. See Hamilton and Tawn,
[doi:10.1111/jedm.70022](https://doi.org/10.1111/jedm.70022) , and the
`brglm2` mean-bias-reduction documentation.

The Firth design has no intercept, tie, positional, or lapse parameter.
Binary comparisons are aggregated in deterministic item/pair order.
Internal contrasts use the last radix-sorted item as reference, then
both estimates and covariance are transformed to sum-to-zero
coordinates. `$vcov` is the model-based inverse expected information at
the bias-reduced estimate, transformed to item coordinates; it is not a
penalized-Hessian or bootstrap covariance. Its rank is the number of
items minus one because of centering. SEs are square roots of its
diagonal. Separation and undefeated or winless items are supported when
the comparison graph is connected.

Firth fits must converge with finite estimates and valid covariance.
Failures error without fallback. A valid fit with zero score variance is
retained: `$reliability` is `NA` and `$ssr$status` is
`"zero_score_variance"`. Other invalid theta/SE or reliability
arithmetic raises an error. Use
[`predict.pairwiseLLM_bt_firth()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_firth.md)
for first-item win probabilities.

## Alpha-adjusted estimation

Hamilton and Tawn
([doi:10.1111/jedm.70022](https://doi.org/10.1111/jedm.70022) , equation
3) define an adjustment to the score equation for item r: \$\$a_r =
\alpha\left(1 - \frac{2}{N-1}\sum\_{j\ne r}p\_{rj}\right).\$\$ With
\\p\_{ij}=\operatorname{logit}^{-1}(\theta_i-\theta_j)\\, observed win
counts \\w\_{ij}\\, and \\c=\alpha/(N-1)\\, the implemented objective is
\$\$\ell\_\alpha(\theta) = \sum\_{i\<j}\\w\_{ij}\log p\_{ij} +
w\_{ji}\log(1-p\_{ij})\\ + c\sum\_{i\<j}\log\\p\_{ij}(1-p\_{ij})\\.\$\$
The penalty covers every unordered pair, including unobserved pairs, and
adds c pseudo-wins in each direction. It differs from sirt's
conventional epsilon adjustment, which uses observed win proportions.
Neither method changes which pairs are selected. Alpha adjustment is
motivated by adaptive scheduling; it is not universally preferred or a
guarantee of unbiased SSR. Firth remains the intended modern comparator
for random schedules. Schedule-aware bootstrap correction is separate
work.

The alpha engine shares Firth's binary input and sum-to-zero convention.
Items are radix-sorted; coefficient i is the contrast of item i to the
last item, for i = 1,...,N-1. If B is the centered reference map, theta
= B beta. For pair design row x and total observed comparisons m, the
negative objective Hessian is
\\H\_\alpha=\sum\_{i\<j}(m\_{ij}+2c)p\_{ij}(1-p\_{ij})xx^T\\. The
original-data information is
\\I=\sum\_{i\<j}m\_{ij}p\_{ij}(1-p\_{ij})xx^T\\. The returned covariance
is \\B I^{-1} B^T\\, evaluated at the alpha estimate, with SEs from its
diagonal. These are model-based SEs conditional on the realized
comparison graph, not schedule-aware uncertainty. The penalized Hessian,
inverse penalized curvature, and sandwich covariance are not used for
reported SEs or SSR. Centering makes the item covariance rank N-1.

A single [`stats::glm.fit`](https://rdrr.io/r/stats/glm.html) IWLS fit
uses weighted binary rows for the augmented counts, zero starts, and a
quasibinomial-logit working family with dispersion fixed at one. This
supplies the exact binomial-logit estimating equations without warnings
about fractional pseudo-counts; no dispersion estimate or GLM covariance
is used. The objective and derivatives are evaluated independently.
Native convergence and full rank are necessary but not sufficient: the
maximum absolute adjusted item score must be at most `gradient_tol`, and
the maximum absolute item-coordinate Newton correction at most
`step_tol`. Both reduced matrices must be positive definite with
reciprocal condition number at least `min_rcond`. All numeric controls
must be positive and finite; `maxit` must be an integer, `min_rcond`
less than one, and `trace` logical.

Fits never change alpha or solver after failure. Numerical validation
errors have class `pairwiseLLM_bt_alpha_error` (also a BT validation
error). Their `theta`, `provenance`, `diagnostics`, and `failure_reason`
fields preserve available results for auditing, including converged
theta if uncertainty fails. No theta-only public fit is returned. A
valid equal-strength fit is retained with `NA` SSR and
`zero_score_variance` status, as for Firth. When every item's total wins
equal its total losses, zero is the exact stationary solution.
`$diagnostics$exact_zero_solution` records its use;
`$diagnostics$coefficients` are the effective contrasts, while `$fit`
retains the raw IWLS output. This is an exact count-based identity, not
rounding small estimates to zero. Native convergence and uncertainty
checks still apply. Extremely small/large penalties can exceed numerical
resolution and error. The dense all-pair design has no large-scale
sparse-optimization guarantee. Use
[`predict.pairwiseLLM_bt_alpha()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_alpha.md)
for plug-in pair probabilities.

## See also

[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

Other frequentist models:
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`predict.pairwiseLLM_bt_alpha()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_alpha.md),
[`predict.pairwiseLLM_bt_firth()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_firth.md),
[`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

## Examples

``` r
# Example using built-in comparison data
data("example_writing_pairs")
bt <- build_bt_data(example_writing_pairs)

if (requireNamespace("sirt", quietly = TRUE)) {
  fit1 <- fit_bt_model(bt, engine = "sirt", sirt_eps = 0.3, verbose = FALSE)
  fit1$ssr
  fit1$provenance$adjustment
}
#> $method
#> [1] "epsilon"
#> 
#> $eps
#> [1] 0.3
#> 
if (requireNamespace("BradleyTerry2", quietly = TRUE)) {
  fit2 <- fit_bt_model(bt, engine = "BradleyTerry2", verbose = FALSE)
}
```
