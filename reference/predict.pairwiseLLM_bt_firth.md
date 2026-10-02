# Predict pairwise win probabilities from a Firth Bradley-Terry fit

Calculate `plogis(theta1 - theta2)`, the probability that the first item
wins. Predictions use the fitted item strengths, without a positional,
lapse, or tie parameter. They do not integrate over estimation
uncertainty.

## Usage

``` r
# S3 method for class 'pairwiseLLM_bt_firth'
predict(object, newdata = NULL, ...)
```

## Arguments

- object:

  A Firth fit returned by
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  with `engine = "brglm2"`.

- newdata:

  A data frame containing `object1` and `object2` item IDs. `NULL` uses
  the original comparisons in their original order. IDs must be
  nonmissing and present in the fitted model. Repeated pairs are
  allowed; comparing an item with itself returns 0.5.

- ...:

  Reserved; additional arguments are rejected.

## Value

A numeric vector of first-item win probabilities in input row order. An
empty data frame returns `numeric(0)`.

## See also

[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

Other frequentist models:
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

## Examples

``` r
if (requireNamespace("brglm2", quietly = TRUE)) {
  comparisons <- data.frame(object1 = c("a", "a", "b"),
                            object2 = c("b", "c", "c"), result = c(1, 1, 1))
  fit <- fit_bt_model(comparisons, engine = "brglm2")
  predict(fit)
  predict(fit, data.frame(object1 = "c", object2 = "a"))
}
#> [1] 0.1120953
```
