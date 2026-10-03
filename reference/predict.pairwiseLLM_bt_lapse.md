# Predict ordered comparisons from an experimental lapse BTL fit

Calculate `(1-epsilon) * plogis(theta1-theta2+beta) + epsilon/2`.
Positive beta favors the first presented item. Swapping the items
generally does not give complementary probabilities unless beta is zero.
These are plug-in probabilities, without integration over estimation
uncertainty. Valid epsilon-zero boundary fits also support prediction;
their unavailable joint uncertainty does not invalidate the point
estimates.

## Usage

``` r
# S3 method for class 'pairwiseLLM_bt_lapse'
predict(object, newdata = NULL, ...)
```

## Arguments

- object:

  A validated experimental fit from
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  with `engine = "lapse"`.

- newdata:

  A data frame containing `object1` and `object2` IDs known to the fit.
  `NULL` uses the original comparisons in their original order. Repeated
  pairs and self-predictions are allowed; a self-prediction includes
  positional bias and need not equal 0.5. Empty input returns
  `numeric(0)`.

- ...:

  Reserved; additional arguments are rejected.

## Value

Numeric first-item win probabilities in input row order.

## See also

[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[integrated CJ
workflow](https://shmercer.github.io/pairwiseLLM/articles/adaptive-cj-workflow.html)

Other frequentist models:
[`bootstrap_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/bootstrap_bt_model.md),
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`predict.pairwiseLLM_bt_alpha()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_alpha.md),
[`predict.pairwiseLLM_bt_firth()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_firth.md),
[`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)
