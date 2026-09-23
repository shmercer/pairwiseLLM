# Predict oriented hub-spoke comparison probabilities

Predict oriented hub-spoke comparison probabilities

## Usage

``` r
predict_link(result, pairs)
```

## Arguments

- result:

  A valid common result returned by
  [`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md).

- pairs:

  Data frame with `observation_id`, `A_set`, `A_item`, `B_set`, and
  `B_item`, using the identities in
  [`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md).
  No outcomes are accepted. A and B encode presentation orientation;
  both directions are legal.

## Value

A double vector of probabilities that presented A wins, aligned with the
input rows. The estimator's prediction hook integrates its uncertainty;
the dispatcher never silently substitutes probabilities at posterior
means.

## Details

Conditional probabilities are
`(1-epsilon) * plogis(theta_A-theta_B+beta) + epsilon/2`. Reversing
presentation is complementary only when beta is zero. Prediction
requires no refit or provider calls. E1 integrates over delta using the
serialized posterior quadrature nodes. It refines numerical integration
for a requested pair when necessary, without updating the posterior or
modifying the result. There is no plug-in-mean prediction mode.
Integration failure raises `pairwiseLLM_e1_numerical_error`, rather than
substituting another prediction. E2 integrates the oriented Gaussian
pair contrast under its joint Laplace approximation, including all
item/offset cross-covariances. Deterministic adaptive quadrature over
the standard Normal variable splits at zero and the logistic transition
(bounded to plus/minus eight SDs to avoid excessively long finite
intervals); both infinite tails are integrated. Degenerate zero-variance
contrasts use the exact conditional probability. Repeated calls do not
sample or mutate state. Integration failure raises
`pairwiseLLM_e2_numerical_error`. E3 MAP uses the same integration with
`pairwiseLLM_e3_numerical_error` on failure. E3-MCMC averages the
conditional probability over its retained raw draws, without new
sampling.
