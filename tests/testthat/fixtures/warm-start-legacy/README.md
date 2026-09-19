# Frozen pre-issue-259 warm-start evidence

`baseline-1.5.1.rds` was captured from unchanged runtime/resources at commit
`013f869b1ba2272b88e85937023d57eb2c2c99fa`, package 1.5.1, R 4.6.1,
glmnet 5.0. Capture command (repository root):

```sh
Rscript --vanilla data-raw/warm-start/capture-issue-259-baseline.R
```

SHA-256:
`77fe4871283d7d938334d99438d5d54f0554c85f02d35ff1f6ba7c1a446433dc`.
The standalone builder verifies the baseline runtime diff, rejects untracked
runtime source/data files (including ignored files, excluding histories/compiled
caches), disables test-helper loading, and verifies the engine version.
It sets all three RNG kinds and uses fixed preparation metadata. It is excluded
from the source package with data-raw, and is never sourced by tests.
It refuses to overwrite the fixture. An optional empty output-directory argument
allows an independent capture for comparison without replacing historical data.

All features/outcomes are synthetic. No text extraction, study data, production
models, provider calls or model-quality selection is involved. Data generation
and case settings are fixed in the builder, not loaded from study files.

| Case | Rows | Contract exercised |
|---|---:|---|
| default | 15 | Public defaults including 41 alphas, seed 1, 5x5 CV, lambda.1se |
| tied_missing | 25 | Tied outcomes, imputation, constant/NZV removal, seed 37, lambda.min |
| one_predictor | 20 | One retained column with glmnet's excluded-zero-column fitting workaround |

Each case stores inputs, full format-1 model, reduced format-2 model, and
predictions on seven separate synthetic rows including missing values. Full
artifacts retain every tuning trace, inner preprocessing record, OOF prediction,
outer observation/prediction and calibration. A full/reduced cross-task ensemble
includes a deliberately repeated component to lock current permissive behavior.
Schema metadata and RNG provenance are retained alongside the cases.

Tests always check legacy loading, deployment, reduction and cross-task behavior
without calling glmnet/Python. Numerical refits use the captured glmnet version
only, with tolerance 1e-8 and exact fold/alpha choices. Different glmnet versions
skip only that historical refit test, not legacy deployment or the existing
engine-reference tests. Deployment comparisons use tolerance 1e-12 to permit
floating-point/BLAS roundoff across platforms. No test regenerates expected data.

Do not refresh these fixtures after refactoring to make a regression pass. Any
deliberate historical recapture must use the baseline code and be reviewed as a
change to compatibility evidence. Inspect new format-3 fields separately; the
tests deliberately compare historical numerical meaning rather than requiring
new public fits to remain format 1.
