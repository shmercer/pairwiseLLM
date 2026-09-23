# Explicit-evidence linking sessions

E1–E3 estimate a separately centered hub shape, a separately centered
spoke shape, and a spoke offset. Choose an estimator explicitly:
`fixed_shape_offset` (E1), `gaussian_posterior_bridge` (E2), or
`joint_offset` (E3). These APIs accept explicit evidence independently
of adaptive pair selection or stopping.

``` r

library(pairwiseLLM)
hub <- list(set_id = "hub", items = data.frame(item_id = c("h1", "h2")))
spoke <- list(set_id = "spoke", items = data.frame(item_id = c("s1", "s2")))
cross <- data.frame(observation_id = c("cross-1", "cross-2"),
  A_set = "hub", A_item = c("h1", "h2"),
  B_set = "spoke", B_item = c("s1", "s2"), y_A = c(0L, 1L))
phase_a <- list(hub = list(points = c(h1 = -1, h2 = 1)),
  spoke = list(points = c(s1 = -.5, s2 = .5)))
judge <- list(beta = 0, epsilon = 0, model_variant = "btl",
  link = "logit", source = "frozen Phase A")
input <- prepare_link_input("fixed_shape_offset", hub, spoke, phase_a,
  cross[1, ], judge)
state <- start_link_session(input)
print(state)
```

    ## Link hub -> spoke: fixed_shape_offset v1 [active]
    ##   Offset: 3.47948 (SD 3.36821); cross_set; valid: TRUE
    ##   Uncertainty: offset_only_conditional_on_fixed_shapes
    ##   Phase A: points/points payloads 2/2; raw rows used 0/0; cross rows 1

``` r

summarize_items(state)
```

    ## # A tibble: 4 × 12
    ##   set_id item_id global_item_id theta_link_mean theta_link_sd theta_link_lower
    ##   <chr>  <chr>   <chr>                    <dbl>         <dbl>            <dbl>
    ## 1 hub    h1      NA                       -1             0               -1   
    ## 2 hub    h2      NA                        1             0                1   
    ## 3 spoke  s1      NA                        2.98          3.37            -2.48
    ## 4 spoke  s2      NA                        3.98          3.37            -1.48
    ## # ℹ 6 more variables: theta_link_upper <dbl>, theta_link_eap <dbl>,
    ## #   estimator_id <chr>, uncertainty_scope <chr>, rank_link <dbl>,
    ## #   link_spoke_id <chr>

E1 consumes only point summaries; its uncertainty is conditional on
those fixed shapes. E2 consumes full posterior item draws, and E3
consumes exact raw within-set outcomes. Both include shape and offset
uncertainty. Historical Phase A artifacts can be supplied with
`list(artifact = artifact)` if they contain the method’s required
payload. Summaries cannot substitute for E2 draws or E3 raw rows. The
artifact’s exact hash is retained, while irrelevant evidence is excluded
from the normalized statistical input.

`theta_link_mean` is a posterior mean for E1 quadrature and E3-MCMC and
a MAP location for E2/E3 Laplace. `theta_link_eap` remains an identical
compatibility alias. Every item carries estimator ID, uncertainty scope,
SD, interval and rank; unavailable uncertainty is `NA`.

``` r

path <- tempfile(fileext = ".rds")
save_link_session(state, path)
restored <- load_link_session(path, input = input)
stopifnot(identical(state, restored))
stopifnot(identical(resume_link_session(restored, input), restored))
next_input <- prepare_link_input("fixed_shape_offset", hub, spoke, phase_a,
  cross, judge)
continued <- resume_link_session(restored, next_input)
summary(continued)[c("estimator_id", "delta_spoke_mean", "delta_spoke_sd",
  "phase_b_active_edges_used", "uncertainty_scope", "fit_valid", "status")]
```

    ## # A tibble: 1 × 7
    ##   estimator_id       delta_spoke_mean delta_spoke_sd phase_b_active_edges_used
    ##   <chr>                         <dbl>          <dbl>                     <int>
    ## 1 fixed_shape_offset        -4.71e-18           1.68                         2
    ## # ℹ 3 more variables: uncertainty_scope <chr>, fit_valid <lgl>, status <chr>

``` r

unlink(path)
```

Resume preserves exact estimator/version, canonical item order,
configuration, Phase A evidence/artifact identity and the old
cross-evidence prefix. Only new cross observations can be appended.
Changing the estimator, numerical controls, judge or Phase A requires a
fresh session. Continuation restores numerical warm-start data, never a
posterior-as-prior update. Passing the identical input is a no-op,
including for explicitly selected E3-MCMC. Saving MCMC sessions
preserves draws and prediction data without requiring the original
CmdStan files.

[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md)
and
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md)
also accept these sessions using a dedicated `link-session.rds` file.
Session directories cannot mix the two formats.
[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md)
returns the common link-stage log. Source counts are distinct from raw
likelihood rows consumed (zero Phase A rows for E1/E2). Unknown external
source hashes/counts remain missing. Computed payload and configuration
hashes are always retained.

For multiple spokes, pass a list of prepared inputs to
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md).
They must share the exact hub evidence, hub identities, judge and
estimator. Each result retains its own hub posterior;
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md)
includes `link_spoke_id` to keep those distinct. No cross-spoke
covariance or pooled hub posterior is invented. Status (`active`,
`probe`, `frozen`) is externally supplied reporting metadata, not a new
stopping rule. Held-out probe outcomes must never be included in the
estimator’s active `cross` table.

For rubric transport, fit
[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md)
with the labeled Phase A hub artifact and
`calibration_design = "linked_anchors"`. Prepare linking inputs with
that exact hub artifact, then pass the common result or session to
[`predict()`](https://rdrr.io/r/stats/predict.html). Global item IDs are
required. Only spoke scores are returned. The frozen hub calibration is
reused, with the original hub mean restoring the reference origin; no
target labels, cohort rescaling or updated hub posterior refit the
calibration. Invalid or unidentified links fail. The prediction’s
`linking` attribute retains estimator identity, uncertainty scope,
evidence provenance and diagnostics.

Saved anchored-joint Phase B sessions are unsupported and fail with a
restart message. Compatible historical Phase A artifacts remain
reusable. Adaptive selection integration and removal of the remaining
old execution code are separate epic work; use the explicit-evidence
APIs above for E1–E3 sessions.

## Citation

Mercer, S. H. (2026). Explicit-evidence linking sessions. In
*pairwiseLLM* \[R package vignette\]. Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.pairwiseLLM>
