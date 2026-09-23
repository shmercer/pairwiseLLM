# Design: Adaptive Linking

Linking places separately ranked sets of writing samples on a common
scale. Within-set comparisons tell us how samples differ within their
set, but cannot tell us how far apart two sets lie. Between-set
comparisons supply that missing information. This article gives the
statistical models behind the [practical
guide](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking.md)
and [worked
examples](https://shmercer.github.io/pairwiseLLM/articles/linking-sessions.md).

Version 1.6.0 supports E1, E2 and E3 with **explicit estimator
selection**. None is the package default. E3-MCMC is an optional
audit/reference engine for E3. The statistical definitions follow the
prospectively frozen [study
contracts](https://github.com/shmercer/pairwise-linking-study/tree/2bf3f0b4a2f257b7855f965f782f05c61853aac6/docs),
including D004 as amended by D006–D009. The later study will select a
default; these implementations and examples do not establish one method
as the winner.

Use
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md)
to validate evidence,
[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md)
to estimate one link, and
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
for persistent work. For Phase A adaptive estimation, see the
[within-set
design](https://shmercer.github.io/pairwiseLLM/articles/within-set-adaptive-design.md).

## Coordinates, identification, and the judge model

Let the hub contain $`N_H`$ items and the spoke contain $`N_S`$ items.
For each set, choose a deterministic orthonormal Helmert basis $`H_k`$
spanning its sum-to-zero subspace:

``` math
H_k^\top H_k=I_{N_k-1},\qquad \mathbf 1^\top H_k=0,
\qquad z_k=H_k u_k,\quad k\in\{H,S\}.
```

The global locations are

``` math
\theta_H=z_H,\qquad \theta_S=\delta\mathbf 1+z_S,
\qquad \delta\sim N(0,5^2).
```

Here, **shape** means the centered differences among a set’s scores. The
**offset** $`\delta`$ shifts the whole spoke relative to the hub.
Centering each Phase A set separately does not provide information about
that shift. The default offset prior is independent of both shapes; a
changed prior must be explicitly requested and must not silently alter
the frozen study’s primary analysis.

For ordered comparison $`r`$, let $`y_r=1`$ mean A wins. Conditional on
the fixed, shared Phase A judge parameters $`\beta`$ (presentation bias)
and $`\epsilon`$ (lapse probability),

``` math
\eta_r=\theta_{A_r}-\theta_{B_r}+\beta,\qquad
p_r=\frac{\epsilon}{2}+(1-\epsilon)\operatorname{logit}^{-1}(\eta_r),
```
``` math
L(D\mid\theta,\beta,\epsilon)
=\prod_{r\in D}p_r^{y_r}(1-p_r)^{1-y_r}.
```

The model is conditional on these judge estimates; their estimation
uncertainty is not propagated. Components absent from the selected BTL
variant must be zero. Within-set contrasts cancel $`\delta`$, so only
cross-set evidence can identify it. With zero cross-set observations,
its posterior remains exactly its prior and identification is reported
as `prior_only`. With $`\epsilon=1`$, every comparison has probability
one half; a positive-budget fit is `unidentified`.

Presentation order matters when $`\beta\ne0`$. Swapping A/B and
replacing $`y`$ by $`1-y`$ preserves the likelihood when $`\beta`$ also
changes sign. Negating all locations, outcomes and $`\beta`$ reverses
the estimated locations and offset. Simply swapping A/B while keeping
nonzero bias fixed does not give complementary probabilities.

## E1: fixed-shape offset

E1 (`fixed_shape_offset`) holds the separately centered Phase A point
estimates $`\widehat z_H^A,\widehat z_S^A`$ fixed and targets

``` math
p(\delta\mid D_X,\widehat z_H^A,\widehat z_S^A)
\propto p(\delta)
L(D_X\mid\widehat z_H^A,\delta+\widehat z_S^A,\beta,\epsilon).
```

Only the between-set comparisons $`D_X`$ enter this likelihood. Earlier
Phase A outcomes are not replayed. This makes E1 a one-dimensional
update: it moves the spoke without changing the relative scores within
either set.

Adaptive Gauss–Kronrod quadrature computes normalized posterior moments
and CDF-based equal-tailed 95% intervals. Both infinite tails are
integrated; a posterior mode can stabilize integration but does not
replace it with a Laplace approximation. Integration failure is an
explicit E1 failure, not a switch to another method. With no informative
cross evidence, the exact prior is returned.

Conditional on the fixed shapes,

``` math
\operatorname{Var}(\theta_H)=0,\qquad
\operatorname{Cov}(\theta_S)=\operatorname{Var}(\delta)\mathbf1\mathbf1^\top,
\qquad \operatorname{Cov}(\theta_H,\theta_S)=0.
```

Thus all spoke uncertainty comes from the same shift. Zero hub variance
is a consequence of conditioning, not evidence that the hub’s original
scores were measured perfectly. The uncertainty scope is
`offset_only_conditional_on_fixed_shapes`.

## E2: full-covariance Gaussian posterior bridge

E2 (`gaussian_posterior_bridge`) starts from joint posterior item draws
from each Phase A fit. Center each draw separately and transform it to
reduced coordinates: $`u_k^{(m)}=H_k^\top z_k^{(m)}`$. Estimate the mean
$`\mu_k`$ and full sample covariance $`\Sigma_k`$, then approximate

``` math
u_k\mid D_k\approx N(\mu_k,\Sigma_k).
```

The Phase B target is

``` math
p(\delta,u_H,u_S\mid D_H,D_S,D_X)
\propto p(\delta)\,
\phi(u_H;\mu_H,\Sigma_H)\phi(u_S;\mu_S,\Sigma_S)
L(D_X\mid H_Hu_H,\delta+H_Su_S,\beta,\epsilon).
```

Phase A information enters through the two Gaussian bridges; only new
cross-set observations enter the likelihood. The off-diagonal
covariances are essential: separately supplied marginal standard errors
cannot reconstruct how the scores vary together. The public input
requires joint draws or compatible artifacts containing them, not a
user-supplied diagonal approximation.

For each reduced covariance, the smallest successful Cholesky jitter is
selected from

``` math
\{0,10^{-12},10^{-10},10^{-8},10^{-6}\}\,s,
```

where $`s`$ is the maximum positive diagonal variance, or one when no
positive scale is available. The selected jitter is recorded. Failure
beyond this bounded ladder invalidates the bridge; no
independent-marginal fallback or coordinate removal is allowed.

E2 finds a MAP estimate using the exact Gaussian penalties and
lapse-mixture likelihood, then obtains Laplace uncertainty from the
observed Hessian. It propagates shape and offset uncertainty
approximately, conditional on the fixed judge. With no informative cross
evidence, it returns the independent stabilized bridges and the offset
prior.

## E3: single-use joint-offset model

E3 (`joint_offset`) returns to the raw hub and spoke Phase A comparisons
and uses independent original shape priors in reduced coordinates:

``` math
u_H\sim N(0,I_{N_H-1}),\qquad
u_S\sim N(0,I_{N_S-1}),\qquad \delta\sim N(0,5^2).
```
``` math
p(\delta,u_H,u_S\mid D_H,D_S,D_X)
\propto p(\delta)p(u_H)p(u_S)
L(D_H\mid H_Hu_H,\beta,\epsilon)
L(D_S\mid H_Su_S,\beta,\epsilon)
L(D_X\mid H_Hu_H,\delta+H_Su_S,\beta,\epsilon).
```

Each observation enters exactly once. These shape priors are the
reduced, sum-to-zero representation of the original iid standard-Normal
raw-location prior. Inputs must be compatible with that Phase A model;
the linker cannot reconstruct or validate an undocumented external prior
from an outcome table.

A Phase A posterior cannot also be used as a prior when its generating
outcomes are included in this likelihood. That would count the same
information twice. Artifact scores and draws can remain part of
provenance, but are not an additional statistical input to E3. E3
therefore requires more original-data retention than E2.

### MAP/Laplace calculation

For E2 and E3, write $`q=(\delta,u_H^\top,u_S^\top)^\top`$, and let
$`\ell(q)`$ be the negative log posterior. At an accepted stationary
mode,

``` math
\widehat q=\arg\min_q\ell(q),\qquad
V_q=\{\nabla^2\ell(\widehat q)\}^{-1}.
```

The implementation uses deterministic multistart optimization in
whitened coordinates, analytic derivatives and gradient/convergence
checks. The observed Hessian must be positive definite without further
regularization. A finite set of starts cannot guarantee finding the
global mode of a multimodal lapse-mixture posterior; failures remain
visible in diagnostics.

With the linear item transformation $`T`$,

``` math
\theta=Tq,\qquad \widehat\theta=T\widehat q,\qquad
V_\theta=TV_qT^\top.
```

Item and offset intervals use the corresponding Normal approximation.
The public `theta_link_mean` field is the MAP/Laplace center for E2 and
E3, and a posterior mean for E1 and E3-MCMC. `theta_link_eap` is an
identical compatibility alias, not a promise that all engines return an
exact posterior expectation.

### E3-MCMC audit engine

Explicit
`control = list(estimator = list(engine = "mcmc", cmdstan = ...))`
requests the optional Stan/CmdStan model with the **same likelihood,
priors, coordinates and fixed judge**. It returns empirical posterior
means, covariance and equal-tailed intervals, with chain-indexed draws
retained for prediction and persistence. Supply a seed for repeatability
within a fixed toolchain.

Sampler diagnostics include R-hat, effective sample sizes, divergences,
treedepth, energy diagnostics and the audit gate. A failed audit does
not silently become a MAP fit or trigger an automatic repair. See
[`?fit_link`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md)
for the exact diagnostic thresholds and controls. The provider-free
real-sampler tests compare Stan log densities to an independent R
objective and check parity on well-behaved examples.

At zero cross edges, offset summaries use the exact independent Normal
prior, with zero between-block covariance. Within-shape moments are
sampled; spoke intervals combine shape draws with the independent offset
distribution. Saving a result preserves the necessary numerical draws
without depending on original CmdStan CSV files.

## Evidence, prediction, and uncertainty contracts

Each judgment has a unique `observation_id`. Genuine repeated judgments
of a pair have different IDs and contribute repeated likelihood
information. Reusing an ID within or across raw evidence blocks is
rejected. E1/E2 consume zero Phase A *likelihood* rows; their
source-observation counts are recorded separately and remain missing
when unknown.

Items are normalized to deterministic ID order. Observation order is
retained: likelihood estimates are invariant to row permutation, but the
versioned `link-v1-rlang` payload hashes are order-sensitive. Exact
artifact hashes, asserted source hashes, normalized evidence hashes and
configuration hashes have different roles; none should be substituted
for another. The producing rlang version and supplied package commit are
recorded.

[`predict_link()`](https://shmercer.github.io/pairwiseLLM/reference/predict_link.md)
returns

``` math
\Pr(A\text{ wins}\mid D)=
\frac{\epsilon}{2}+(1-\epsilon)
E\left[\operatorname{logit}^{-1}(\theta_A-\theta_B+\beta)\mid D\right].
```

E1 integrates over its offset posterior; E2/E3 Laplace integrate the
Gaussian contrast distribution; E3-MCMC averages over retained draws.
Prediction accepts identities and presentation order, without outcomes.
Missing uncertainty is `NA`, never silently replaced by zero. E2/E3
uncertainty is labeled `joint_shapes_and_offset`, conditional on the
fixed judge and model assumptions.

## Selection, probes, and stopping

Link estimation accepts explicit evidence independently of pair
selection. Adaptive Phase B D-optimal execution is unavailable for all
estimators pending separate selector validation. Having a covariance
matrix does not validate a selection rule. Phase A and ordinary
within-set adaptive ranking remain available.

Held-out probe and final-test observations stay outside the fitted
`cross` table. Source partitioning is the caller’s responsibility,
particularly when Phase A is supplied only as summaries or draws.
Internal probe checks reject overlap in IDs and unordered base pairs,
including reversed presentations. Prediction caches bind to the
estimator, spoke, evidence and input identity.

Internal adapters supply covariance and gradients of the ordered latent
contrast $`\theta_A-\theta_B`$, not gradients of posterior-average
probabilities. Their per-refit stopping assessments do not enable
automatic stopping. Prior-only or invalid fits, missing history, and
unavailable required uncertainty block those assessments. E1’s
conditional uncertainty cannot satisfy a full-uncertainty reliability
criterion. Session statuses `active`, `probe` and `frozen` are reporting
metadata; changing a status does not remove likelihood rows.

## Persistence, multiple spokes, and rubric transport

Sessions preserve exact results, hashes, diagnostics and continuation
inputs. Unchanged resume is a no-op. New evidence must extend the old
ordered prefix; Phase A, estimator, judge and numerical settings must
remain the same. Refitting uses the original evidence/priors and the
cumulative cross table, never a previous Phase B posterior as a new
prior.

Each hub/spoke fit is separate. E2/E3 can update the hub differently for
different spokes. Reports preserve spoke identity; they do not invent a
pooled hub posterior or cross-spoke covariance. Rubric transport uses
the exact original Phase A hub calibration, restoring its original
location origin. It conditions on linked point locations and does not
propagate CJ uncertainty through the rubric model.

### Frozen standalone rubric-reference identity

[`prepare_linked_rubric_reference()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_linked_rubric_reference.md)
extends the reference source contract without changing E1–E3 or the
conditional ordinal likelihood. A completed standalone fit must carry
versioned evidence identity recorded at fit time. Each refit records its
own selected observations, including sampled subsets. Constructor
evidence must reproduce that identity exactly. Retrospectively supplied
compatible rows cannot verify a legacy fit; such fits require refitting
for reusable references, while their existing same-set uses remain
supported.

Canonical evidence retains the comparison identifier, ordered item
endpoints, binary outcome, phase and normalized judge scope, with
repeated rows retained. It sorts observations deterministically, binds
the fitted item domain, and strips incidental table attributes.
Collection timestamps and table row names do not change statistical
identity. The constructor translates these rows into uniquely identified
within-hub observations for E3; evidence is never aggregated into
pairwise counts or discarded merely because comparisons repeat.

The computed reference hash binds set/source/global item identities,
accepted locations and SDs, aligned posterior draws, trait,
higher-is-better orientation, shared judge settings, native
model/prior/configuration identity, and exact evidence/count. Fit
diagnostics and reliability are retained. Descriptive user provenance is
separate and cannot override computed fields. Failed standalone
diagnostics retain the existing rubric warning policy; adaptive import
readiness is not imposed on standalone fits. Existing adaptive-artifact
validation and artifact hashes remain unchanged.

The standalone `source$reference_hash` identifies one frozen reference
for all estimators. E1 receives its accepted points, E2 its joint draws,
and E3 its raw observations. Transport verifies both source identity and
the corresponding actual hub payload, including the origin removed
during centering, exact item mapping, and shared judge settings. An
asserted source hash alone is insufficient. These hashes establish
consistency of saved objects, not cryptographic proof of how an
externally manufactured R object was obtained.

For frozen hub locations $`m_{H,i}`$, linked spoke locations are
translated back by $`\bar m_H`$ before the saved ordinal calibration is
applied. E2/E3 updates to the hub posterior do not replace $`m_H`$,
refit the ordinal calibration, or introduce target labels. The
calibration remains conditional on accepted point locations; CJ
posterior uncertainty is not propagated through its likelihood. Valid
cross-set identification remains mandatory. The saved reference and
calibration contain data needed for transport without live sampler
objects.

## Why old Phase B sessions cannot resume

The removed anchored-joint method used Phase A information both in
data-derived priors and replayed likelihood, and treated separately
centered locations as information about the global offset. E1–E3 enforce
single-use evidence and explicit offset identification instead.

Saved anchored-joint Phase B states raise
`pairwiseLLM_unsupported_legacy_link_state`. They cannot be
reinterpreted as E1–E3 posteriors. Compatible historical Phase A
artifacts may be reused if they contain the chosen method’s required
information. Restart with those original inputs and intentionally
supplied cross-set observations; there is no hidden legacy fallback.

## Citation

Mercer, S. H. (2026). *Design: Adaptive Linking* \[R package vignette\].
Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.pairwiseLLM>
