# Inspect explicit-evidence linking results and sessions

Inspect explicit-evidence linking results and sessions

## Usage

``` r
# S3 method for class 'pairwiseLLM_link_result'
summary(object, ...)

# S3 method for class 'pairwiseLLM_link_session'
summary(object, ...)

# S3 method for class 'pairwiseLLM_link_result'
print(x, ...)

# S3 method for class 'pairwiseLLM_link_session'
print(x, ...)
```

## Arguments

- object, x:

  A common linking result or linking session.

- ...:

  Unused.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns one row per
spoke, including offset, uncertainty scope, evidence counts, validity
and controller status. [`print()`](https://rdrr.io/r/base/print.html)
returns its input invisibly.
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md)
supplies linked item means, SDs, intervals, ranks, estimator ID and
uncertainty scope.

## Details

`theta_link_eap` is a compatibility alias for `theta_link_mean`: a
posterior mean for E1 quadrature and E3-MCMC, and the MAP location for
E2/E3 Laplace. Unavailable uncertainty remains `NA`. E1 uncertainty is
conditional on fixed Phase A shapes; E2/E3 include shapes and offset.

## See also

[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md),
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
[`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)

Other linking:
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
[`predict_link()`](https://shmercer.github.io/pairwiseLLM/reference/predict_link.md),
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`save_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md),
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
