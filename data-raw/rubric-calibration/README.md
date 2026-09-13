# Rubric calibration recovery smoke study

From the repository root, run:

```sh
Rscript --vanilla data-raw/rubric-calibration/simulation-recovery.R /tmp/rubric-smoke
```

This source-only, offline script exercises the public rubric fit, prediction,
and evaluation APIs on synthetic completed CJ results. It requires the development
tools used to load this checkout and `withr`. Install `ordinal` and `mgcv >= 1.9-4`
to exercise their optional calibration methods; missing backends produce recorded
failures without substituting another method. `data-raw` is already excluded from
the package build. No provider credentials, network calls, or CJ estimation run.

The bounded default contains 16 prespecified scenarios, each evaluated with all
three methods. It spans K = 3–6, N = 400–1,600, labeled fractions of 5%, 10%, 20%,
30%, and 50%, category imbalance, threshold spacing, linear/compressed/S-shaped/
weak effects, random and score-stratified sampling, and centrally concentrated
calibration labels. These are illustrative conditions, not a factorial study.
To extend the study, source the script, edit the data frame returned by
`rubric_smoke_scenarios()`, and pass it to `run_rubric_smoke(output_dir, scenarios)`.
The scenario seed controls labels, sampling, and score perturbations; each scenario
restores the caller's RNG state. Fits never select categories or methods based on
their results, and missing training categories remain explicit failures.

Outputs are `summary.csv`, `results.rds`, and `session-info.txt` in the supplied
directory. Existing files with those names are replaced on rerun. Records retain
scenario metadata, seeds, category counts, backend/R versions, convergence status,
warnings/failures, extrapolation, normalized RPS, log loss, hard-score metrics,
probability error against generating truth, and linear parameter error where
meaningful. The RDS also retains cumulative calibration summaries, diagnostics,
transformations/ranges, and accuracy for extreme categories. Nonconverged rows
remain explicitly marked and must not be interpreted as successful recovery.

Only rubric labels are held out: evaluation items already belong to the completed
CJ set. Percentile scoring uses the full CJ distribution and supplies no category
probabilities; its RPS/log loss and probability recovery fields are unavailable.
Linked transport is validated separately in test 9200 using the existing Phase B
estimator and reference transformation.

Synthetic theta noise and symmetric draws illustrate sensitivity to predictor
precision. They do not reproduce comparisons per response, adaptive selection,
posterior inference, or network connectivity. Calibration conditions on accepted
point scores; no uncertainty propagation or interval-coverage study is performed.
In noisy scenarios, parameter error is relative to the latent generating relation
and can reflect measurement error. These smoke results establish neither a
universal minimum calibration sample size nor a preferred method or tuning rule.
Probability-oracle recovery and exact invariants are tested separately in 9203.
