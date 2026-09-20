# Bundled warm-start models

**No pretrained models are shipped.** Bundling infrastructure supports reviewed
models, but real model fitting and publication are separate maintainer work.
Synthetic tests and documentation examples do not establish predictive validity.

Installed artifacts are read-only `<normalized-name>.rds` files. Use
`load_warm_start_model(name = ..., source = "bundled")` and ordinary `predict()`.
Bundled and user models share the same classes, validators, RDS reader, and prediction
methods. Prediction from precomputed features requires neither a fitting backend nor Python.
User models belong under `tools::R_user_dir("pairwiseLLM", "data")`, never here.

## Manifest version 1

`manifest.json` contains `manifest_version: 1` and an `artifacts` array (currently empty).
The maintainer workflow generates the canonical JSON representation; do not hand-edit records.
Each record contains:

| Field | Meaning |
| --- | --- |
| `name`, `filename` | Canonical registry identity and exactly `<name>.rds` |
| `metadata` | Name, version, domain, license, provenance note (`notes`), extraction provenance |
| `artifact_type`, `format_version` | `model`, cross-task `ensemble`, or same-task `algorithm_ensemble`, and its independent storage format |
| `schema`, `target` | Frozen schema and standardized outcome definition |
| `built_at` | Explicit UTC `YYYY-MM-DDTHH:MM:SSZ` build timestamp |
| `size_bytes` | Actual xz-compressed RDS size |
| `checksum` | Algorithm `md5` and lowercase digest of the saved file |
| `components` | Ordered array of component records, including one for a single model |

Each component record contains its stable `component_name`, publication `metadata`,
model `format_version`, `training`, `tuning`, and `validation` summaries drawn directly
from the embedded artifact. Training includes task label, n, engine/package versions,
and engine-specific settings: alpha/lambda for glmnet, component count for PLS,
and cost/gamma/epsilon for RBF-SVR. Format-3 components retain compact CV identity.
Tuning records the corresponding engine's settings and conventions. Validation
records nested fold counts, outer metrics, undefined reasons, and warning count.
Undefined metrics remain JSON null, never zero.

Cross-task ensembles report component metrics without an inferred common validation
score or pooled n. Same-task algorithm ensembles additionally retain common task/CV
identity, common n, and ensemble validation derived from aligned outer-held-out
calibrated predictions. Component names and engine versions remain ordered metadata.
Neither ensemble uses disagreement to determine Bayesian prior SD.

Extraction provenance is a named mapping. It records versions/evidence captured during
extraction, supplied cache provenance, or explicit `status: unavailable`. Schema
metadata alone does not attest to extraction. An ensemble points to its components'
provenance. Publication metadata must not expose private dataset paths or identifiers.

Named bundled lookup and listing verify manifest version/structure, unique canonical
names, the complete RDS inventory, path containment, checksum, size, and agreement
between the manifest and the validated artifact. Missing, unlisted, or inconsistent
bundles fail clearly. Same-name user/bundled collisions still require explicit source
selection. Explicit-path loads use ordinary artifact validation without a manifest.
MD5 detects file changes; it does not authenticate publishers. Load trusted RDS files.

## Publication and size review

Use the maintainer workflow in `data-raw/warm-start/` in the source repository.
The existing builder develops v1/glmnet cross-task models. It saves full audits
privately and stages reduced candidates for review. Reducing legacy format 1 gives
model format 2; reducing current format 3 keeps format 3 with summary-only status.
Both ensemble types retain their own ensemble format 1. Construct a same-task
algorithm ensemble from full format-3 components before reducing it; reduced
components cannot establish the row-level evidence for a new ensemble.
Neither ordinary user storage nor registration automatically removes audit evidence.

Audit omission removes row IDs, outcomes, folds, traces, and warning strings. Remaining
labels, notes, diagnostic reasons, metadata, and attributes still require recursive
review; reduction is not anonymization. RBF-SVR retains the numeric support-vector
matrix required for deployment, including its dimensions and feature order. The manifest contains retained summaries whose
full supporting evidence must remain in maintainer-controlled storage.

Review each artifact's compressed size, the combined bundle size, and the resulting
source archive before distribution. The workflow reports bytes and requires explicit
size review, without inventing a universal model-size or predictive-performance cutoff.
Do not put training data, caches, full audits, Python environments, language-model
binaries, or publication review reports in this directory. The only intended resources
are this README, the manifest, and reviewed RDS artifacts.
