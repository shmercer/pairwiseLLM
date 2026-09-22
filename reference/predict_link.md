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
requires no refit or provider calls. Engines are added in subsequent
issues.
