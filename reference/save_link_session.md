# Save and load exact linking sessions

RDS preserves exact types, item/evidence order, hashes, covariance,
numerical modes and prediction data, including E3-MCMC draws. Reading
never refits or migrates a posterior. Only open trusted RDS files.
Unknown external Phase A provenance remains missing; computed payload
hashes are always present.

## Usage

``` r
save_link_session(state, path, overwrite = FALSE)

load_link_session(path, input = NULL)
```

## Arguments

- state:

  A
  [`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
  result.

- path:

  File path for an RDS session.

- overwrite:

  Whether to replace an existing file.

- input:

  Optional expected prepared input (or list of inputs) on load. It must
  match the saved input exactly. Use
  [`resume_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
  to append new evidence after this identity check.

## Value

Saving invisibly returns `path`; loading returns the identical session.

## See also

[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md),
`save_link_session()`

Other linking:
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md),
[`predict_link()`](https://shmercer.github.io/pairwiseLLM/reference/predict_link.md),
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md),
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md),
[`summary.pairwiseLLM_link_result()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_link_result.md)
