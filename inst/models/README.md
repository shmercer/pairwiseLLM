# Bundled warm-start models

Task 07 supplies bundling infrastructure. **No pretrained models are shipped.**
Real model fitting and publication are deferred until after the warm-start series.
Synthetic fixtures live only in tests and do not establish predictive validity.

Installed artifacts are read-only `<normalized-name>.rds` files. Use
`load_warm_start_model(name = ..., source = "bundled")` and ordinary `predict()`.
Bundled and user models share the same classes, validators, RDS reader, and prediction
methods. Prediction from precomputed features requires neither glmnet nor Python.
User models belong under `tools::R_user_dir("pairwiseLLM", "data")`, never here.

## Manifest version 1

`manifest.json` contains `manifest_version: 1` and an `artifacts` array (currently empty).
The maintainer workflow generates the canonical JSON representation; do not hand-edit records.
Each record contains:

| Field | Meaning |
| --- | --- |
| `name`, `filename` | Canonical registry identity and exactly `<name>.rds` |
| `metadata` | Name, version, domain, license, provenance note (`notes`), extraction provenance |
| `artifact_type`, `format_version` | Model or ensemble and its independent storage format |
| `schema`, `target` | Frozen schema and standardized outcome definition |
| `built_at` | Explicit UTC `YYYY-MM-DDTHH:MM:SSZ` build timestamp |
| `size_bytes` | Actual xz-compressed RDS size |
| `checksum` | Algorithm `md5` and lowercase digest of the saved file |
| `components` | Ordered array of component records, including one for a single model |

Each component record contains its stable `component_name`, publication `metadata`,
model `format_version`, `training`, `tuning`, and `validation` summaries drawn directly
from the embedded artifact. Training includes task label, n, selected alpha/lambda,
nonzero count, and engine/package versions. Tuning records seed, alpha grid, lambda
rule, and conventions. Validation records nested fold counts, outer metrics, undefined
reasons, and warning count. Undefined metrics remain JSON null, never zero.
Ensembles have component metrics, not an inferred ensemble validation score or pooled n.

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
It saves full audits privately, explicitly reduces components to summary-only model
format 2, and stages candidates for review. Ensembles retain ensemble format 1.
Neither ordinary user storage nor registration automatically removes audit evidence.

Audit omission removes row IDs, outcomes, folds, traces, and warning strings. Remaining
labels, notes, diagnostic reasons, metadata, and attributes still require recursive
review; reduction is not anonymization. The manifest contains retained summaries whose
full supporting evidence must remain in maintainer-controlled storage.

Review each artifact's compressed size, the combined bundle size, and the resulting
source archive before distribution. The workflow reports bytes and requires explicit
size review, without inventing a universal model-size or predictive-performance cutoff.
Do not put training data, caches, full audits, Python environments, language-model
binaries, or publication review reports in this directory. The only intended resources
are this README, the manifest, and reviewed RDS artifacts.
