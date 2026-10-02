# Predict pairwise win probabilities from an alpha-adjusted Bradley-Terry fit

Calculate `plogis(theta1 - theta2)`, the probability that the first item
wins. These plug-in probabilities do not integrate over uncertainty and
have no tie, lapse, or positional parameter.

## Usage

``` r
# S3 method for class 'pairwiseLLM_bt_alpha'
predict(object, newdata = NULL, ...)
```

## Arguments

- object:

  An alpha-adjusted fit from
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  with `engine = "alpha"`.

- newdata:

  A data frame containing `object1` and `object2` item IDs. `NULL` uses
  the original comparisons in their original order. IDs must be
  nonmissing and present in the fitted model. Repeated pairs are
  allowed; comparing an item with itself returns 0.5.

- ...:

  Reserved; additional arguments are rejected.

## Value

A numeric vector of first-item win probabilities in input row order.

## See also

[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

Other frequentist models:
[`bootstrap_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/bootstrap_bt_model.md),
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`predict.pairwiseLLM_bt_firth()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_firth.md),
[`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

## Examples

``` r
comparisons <- data.frame(object1 = c("a", "a", "b"),
                          object2 = c("b", "c", "c"), result = c(1, 1, 1))
fit <- fit_bt_model(comparisons, engine = "alpha", alpha = 0.5)
predict(fit)
#> [1] 0.7586094 0.9080573 0.7586094
predict(fit, data.frame(object1 = "c", object2 = "a"))
#> [1] 0.09194274
```
