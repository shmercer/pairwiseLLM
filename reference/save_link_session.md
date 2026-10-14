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

## Details

Hashes are verified using the current rlang engine. A failed check with
recorded `link-v1-rlang` provenance from before rlang 1.3.0 raises
`pairwiseLLM_link_hash_incompatibility` (also a linking-contract error),
with `stored_hash_scheme`, `stored_hash_engine_version`, and
`current_hash_engine_version` condition fields. Other mismatches remain
integrity/contract errors; a different version alone is not proof of
incompatibility. Missing engine provenance cannot establish the cause.

Keep the original artifact and hashes. Explicitly prepare new inputs
from the original saved judgments and compatible Phase A points/draws
using
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md).
This preparation needs no new LLM judgments or sampling. A new
standalone reference can reuse a saved completed fit through
[`prepare_linked_rubric_reference()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_linked_rubric_reference.md)
only if its fit-time evidence identity verifies. Otherwise investigate
in its producing environment before deciding separately whether a refit
is needed. Creating a new E3 result itself refits the saved outcomes;
MCMC remains an explicit, potentially expensive choice.
Loading/validation never rebuilds, migrates hashes, overwrites
artifacts, contacts providers, or starts sampling.

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
