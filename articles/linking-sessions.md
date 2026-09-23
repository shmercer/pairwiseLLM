# Guide: Linking Saved Comparisons

This guide shows how to put two separately ranked sets of writing
samples on a common scale using comparisons you already have. It uses
small, invented data and makes no provider requests. You need only
`pairwiseLLM` for the runnable examples.

The reference set is the **hub** and the other set is the **spoke**.
Their original, separate rankings are called **Phase A**. Linking them
is called **Phase B**. Read the [linking
overview](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking.md)
for help choosing inputs, or the [design
guide](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking-design.md)
for the statistical models.

## Start with the samples and their comparisons

Each set has two samples. The IDs below identify samples within their
own set. Add unique `global_item_id` values if you will later reuse a
rubric calibration.

``` r

library(pairwiseLLM)
hub <- list(set_id = "hub", items = data.frame(item_id = c("h1", "h2")))
spoke <- list(set_id = "spoke", items = data.frame(item_id = c("s1", "s2")))
cross <- data.frame(
  observation_id = c("cross-1", "cross-2"),
  A_set = "hub", A_item = c("h1", "h2"),
  B_set = "spoke", B_item = c("s1", "s2"), y_A = c(0L, 1L)
)
```

Each row describes one judgment: `y_A = 1` means sample A won and
`y_A = 0` means sample B won. Retain the order in which samples were
presented. Give a new judgment a new ID even when the same pair is
compared again. Keep any comparisons reserved for checking accuracy
outside this table.

For this example, assume there is no presentation-order bias or random
lapse. For an analysis, use the shared judge settings estimated in Phase
A; keep them fixed when linking.

``` r

judge <- list(beta = 0, epsilon = 0, model_variant = "btl",
  link = "logit", source = "invented example: no bias or lapses")
```

## Choose a method and fit the first comparison

There is no default method. We start with E1 (`fixed_shape_offset`) to
illustrate the workflow. It keeps the original within-set score
differences fixed and estimates how much to shift the spoke scores.

``` r

phase_a <- list(
  hub = list(points = c(h1 = -1, h2 = 1)),
  spoke = list(points = c(s1 = -.5, s2 = .5))
)
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

Higher `theta_link_mean` values mean stronger estimated writing on the
chosen trait. `theta_link_sd` describes uncertainty; it is not a rubric
grade. For E1, this uncertainty concerns only the shift between sets.
The earlier scores are treated as fixed, so the hub’s zero uncertainty
here does **not** mean its original scores were known perfectly.

`theta_link_eap` is another name for `theta_link_mean`, retained for
compatibility. E1 reports posterior averages. E2 and the usual E3 fit
report the most probable joint scores, with approximate uncertainty. See
the design guide before comparing uncertainty across methods.

## Save your work and add the next comparison

``` r

path <- tempfile(fileext = ".rds")
save_link_session(state, path)
restored <- load_link_session(path, input = input)
stopifnot(identical(state, restored))

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

`delta_spoke_mean` is the estimated shift from hub to spoke; positive
values put the spoke higher. `delta_spoke_sd` describes uncertainty in
that shift. `phase_b_active_edges_used` counts judgments used for
fitting, including legitimate repeated judgments. Check `fit_valid` and
the uncertainty description before interpreting the scores.

When adding data, supply all previous rows in their original order
followed by the new rows. Keep the same Phase A inputs, method and
settings. Reusing unchanged input returns the saved fit without fitting
again. Start a new session if you need to change those inputs or
settings. `adaptive_get_logs(continued)` provides the record of the fits
made along the way.

## Use saved uncertainty with E2

E2 (`gaussian_posterior_bridge`) uses joint posterior draws from each
Phase A fit. Each row is one plausible set of scores; columns identify
samples. The tiny matrices below are invented to demonstrate the
required format, not recommended sample sizes.

``` r

hub_draws <- cbind(h1 = c(-1, -2, -.5, -1.5), h2 = c(1, 2, .5, 1.5))
spoke_draws <- cbind(s1 = c(-.3, -.7, -.4, -.6), s2 = c(.3, .7, .4, .6))
e2_input <- prepare_link_input("gaussian_posterior_bridge", hub, spoke,
  phase_a = list(hub = list(draws = hub_draws), spoke = list(draws = spoke_draws)),
  cross = cross, judge = judge)
e2_fit <- fit_link(e2_input)
e2_fit$offset
```

    ## $delta_mean
    ## [1] -1.562499e-13
    ## 
    ## $delta_sd
    ## [1] 1.517156
    ## 
    ## $delta_lower
    ## [1] -2.973571
    ## 
    ## $delta_upper
    ## [1] 2.973571
    ## 
    ## $identification
    ## [1] "cross_set"

E2 can update both sets’ scores and the shift between them. It needs the
joint draws to retain relationships among uncertain scores. Separate
standard errors cannot replace them. It does not fit the original Phase
A comparisons a second time.

## Return to the original comparisons with E3

E3 (`joint_offset`) fits the original within-set comparisons together
with the between-set comparisons, using each judgment once. It requires
compatibility with the original Phase A model, including its prior
assumptions. Saved scores alone are insufficient. These invented
within-set rows use the same format as `cross`.

``` r

within_hub <- data.frame(observation_id = c("hub-1", "hub-2"),
  A_set = "hub", A_item = "h1", B_set = "hub", B_item = "h2", y_A = c(0L, 1L))
within_spoke <- data.frame(observation_id = c("spoke-1", "spoke-2"),
  A_set = "spoke", A_item = "s1", B_set = "spoke", B_item = "s2", y_A = c(0L, 0L))
e3_input <- prepare_link_input("joint_offset", hub, spoke,
  phase_a = list(hub = list(observations = within_hub),
    spoke = list(observations = within_spoke)), cross = cross, judge = judge)
e3_fit <- fit_link(e3_input)
e3_fit$offset
```

    ## $delta_mean
    ## [1] 0
    ## 
    ## $delta_sd
    ## [1] 1.360828
    ## 
    ## $delta_lower
    ## [1] -2.667173
    ## 
    ## $delta_upper
    ## [1] 2.667173
    ## 
    ## $identification
    ## [1] "cross_set"

The usual E3 fit needs no sampler. An optional MCMC engine provides a
more expensive statistical audit and requires CmdStan. It must be
requested explicitly; see
[`?fit_link`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md)
and the design guide. It is not selected automatically when a fit fails.

The three examples illustrate input formats. Their invented Phase A
summaries were not estimated from the same comparisons, so their
numerical results should not be used to judge which method works best.

## Link a prepared standalone reference

For rubric scoring, the hub can be an import-ready adaptive Phase A
result or a standalone Bayesian ranking saved with
[`prepare_linked_rubric_reference()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_linked_rubric_reference.md).
The [rubric
guide](https://shmercer.github.io/pairwiseLLM/articles/rubric-calibration.html#prepare-a-reference-from-a-standalone-ranking)
shows how to prepare and save the latter, including rankings based on
pooled historical comparisons.

A prepared `reference` provides the matching hub inputs for each method:

| Method | Hub input |
|----|----|
| E1 | `list(points = reference$points, source = reference$source)` |
| E2 | `list(draws = reference$posterior_draws, source = reference$source)` |
| E3, including its MCMC engine | `list(observations = reference$evidence, source = reference$source)` |

Always carry `reference$source` with the hub input. It identifies the
frozen reference across methods. The package also checks the actual hub
inputs before applying the saved rubric calibration; copying an
identifier onto different scores or comparisons will not work.

Here is an E2 example using a prepared reference and an existing target
Phase A artifact. `active_cross` must use the set and sample IDs in
these inputs.

``` r

standalone_input <- prepare_link_input(
  "gaussian_posterior_bridge",
  hub = reference$hub,
  spoke = list(set_id = target_phase_a$set_id,
    items = target_phase_a$items[c("item_id", "global_item_id")]),
  phase_a = list(
    hub = list(draws = reference$posterior_draws, source = reference$source),
    spoke = list(artifact = target_phase_a)
  ),
  cross = active_cross, judge = reference$judge
)
standalone_link <- fit_link(standalone_input)
target_scores <- predict(standalone_calibration, standalone_link)
```

Use compatible model settings for the target ranking. `reference$judge`
keeps the shared bias and lapse settings from the reference fit. E3 also
requires compatible original prior assumptions, as described in the
design guide. You do not need target rubric labels for linking or
prediction. E2 and E3 may update hub scores during linking, but the
saved rubric calibration stays attached to the original reference.
Prediction restores that reference’s score origin; it does not adjust
the scores to match the new cohort’s distribution.

## Predict a comparison and check the result

To predict a winner, provide sample identities in their presentation
order, without an outcome column:

``` r

pairs_to_predict <- cross[, c("observation_id", "A_set", "A_item", "B_set", "B_item")]
predict_link(e3_fit, pairs_to_predict)
```

    ## [1] 0.5 0.5

``` r

e3_fit$diagnostics[c("fit_valid", "failure_code", "uncertainty_scope")]
```

    ## $fit_valid
    ## [1] TRUE
    ## 
    ## $failure_code
    ## [1] NA
    ## 
    ## $uncertainty_scope
    ## [1] "joint_shapes_and_offset"

Predictions are probabilities that A wins. They average over the
method’s estimated uncertainty. Keep observed outcomes for checking
those predictions in a separate table. An offset fitted without
between-set comparisons reflects only its starting assumption and does
not establish a link. Failed fits or unavailable uncertainty need
investigation before you use the results.

## Use saved Phase A results or several spokes

Instead of supplying points, draws or raw observations directly, you can
supply `list(artifact = artifact)` for each set’s Phase A input. Read
saved files with [`readRDS()`](https://rdrr.io/r/base/readRDS.html)
first. The artifact must contain what the selected method needs and must
describe the original Phase A fit. Compatible older Phase A results
remain usable; linked Phase B results cannot substitute for them.

For several spokes, prepare one input per spoke and pass the list to
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md).
All must share the same hub information, judge settings and estimator.
Reports include `link_spoke_id`: keep the separate hub estimates
associated with their spokes. The package does not combine them into one
hub fit.

The optional status labels `active`, `probe` and `frozen` help record
your workflow. They do not decide when enough comparisons have been
collected. A frozen spoke must be explicitly reactivated before adding
observations. Automatic Phase B selection and stopping remain
unavailable pending validation.

The [rubric
guide](https://shmercer.github.io/pairwiseLLM/articles/rubric-calibration.md)
explains how to apply a saved hub calibration to linked spoke scores.
Old anchored-joint Phase B sessions must be restarted from compatible
Phase A results; they cannot be resumed as E1, E2 or E3.

## Citation

Mercer, S. H. (2026). *Guide: Linking Saved Comparisons*. In
*pairwiseLLM* \[R package vignette\]. Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.pairwiseLLM>
