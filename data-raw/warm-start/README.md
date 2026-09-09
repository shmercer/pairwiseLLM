# Warm-start maintainer resources

The existing feature inventory, audit, frozen environment lock, and explicit setup
script remain the Task 01 sources of evidence. Installed extraction setup instructions
are in `inst/python/README.md`. The frozen schema is not re-curated by model building.

Task 07 adds `build-bundled-models.R`. Sourcing it only defines functions. It uses public
training, ensemble, preparation, save/load, and prediction APIs. Internal helpers are
used only for the shared manifest contract and path checks. No workflow runs during
package load, installation, examples, or checks. `data-raw/` is excluded from packages.

**Real models are deferred.** After finishing the tasklist series, fit each approved
assessment separately and review its validation/provenance before adding it. Do not
pool independent BTL scales. No bundled model's validity is established by synthetic
tests, a successful fit, or successful ensemble construction.

## Private inputs and configuration

Use an installed/development-loaded version of pairwiseLLM containing Task 07, plus
optional glmnet/withr for training. Text extraction additionally needs the explicitly
configured supported Python environment. No software is installed automatically.

Create a private RDS configuration file with this structure (paths below are placeholders,
not defaults). All dataset/output locations are explicit; keep them outside the source
and installed package trees. Stage and audit directories must be separate and empty.

```r
publication <- function(name, domain) {
  list(name = name, version = "1", domain = domain,
       license = "REPLACE with approved artifact distribution terms",
       notes = "REPLACE with a public-safe provenance and limitations summary")
}
config <- list(
  built_at = "2026-09-08T00:00:00Z", # Set once for this reproducible build.
  stage_dir = "/private/build/candidate",
  audit_dir = "/private/build/evidence",
  metadata = publication("writing-ensemble", "REPLACE with intended domain"),
  tasks = list(
    assessment_a = list(
      data_path = "/private/data/assessment-a.rds",
      task_id = "assessment-a", # Public-safe assessment label, not a student ID.
      metadata = publication("assessment-a-model", "REPLACE with source domain"),
      tuning = list(seed = 1L, outer_folds = 5L, inner_folds = 5L,
                    alpha_grid = seq(0, 1, by = 0.025), lambda_rule = "lambda.1se")
    ),
    assessment_b = list(
      data_path = "/private/data/assessment-b.rds",
      task_id = "assessment-b",
      metadata = publication("assessment-b-model", "REPLACE with source domain")
    )
  )
)
saveRDS(config, "/private/build/config.rds")
```

Each task dataset is a list with `ids`, positional finite `theta`, and exactly one of:

- `features`: the raw canonical feature table, including `item_id` and its
  `warm_start_schema` attribute. Optional `extraction_provenance` is a nonempty named
  character vector captured when the cache was created. RDS preserves the schema
  attribute. Missing provenance remains `status = "unavailable"`; do not attest to a
  cached dataset by checking today's Python environment.
- `texts`: a character vector in ID order. Supply `python` in that task's configuration
  when selecting an interpreter. The workflow captures status/version evidence during
  this extraction run, records environment/lock hashes, and saves the feature cache and
  fuller environment evidence privately. Runtime extraction verifies the supported
  resources; the full lock is not itself a list of runtime version gates.

Task input must represent one assessment per model. Stable task-list names determine
ensemble component order; task labels are provenance, not evidence of independence.
One task produces a single model; two or more produce an equal-weight ensemble.
Omitted tuning fields use the public defaults shown above. Explicit overrides use the
existing public training contract. Insufficient folds fail without automatic reduction.
Configuration metadata cannot replace actual/unknown extraction provenance.

## Build and review

Run explicitly from the source checkout:

```r
source("data-raw/warm-start/build-bundled-models.R")
result <- build_bundled_models("/private/build/config.rds")
review <- readRDS(result$review_path)
review$artifact       # Outer nested-CV metrics and actual component training settings.
review$total_bytes   # Compressed deployment RDS bytes.
review$inspection    # Every retained character field and any restricted-value findings.
```

The build retains full component and ensemble audits privately and generates one staged
artifact plus its manifest. It compares numeric predictions before/after recursive
audit omission and complete prediction objects after RDS round trips. Public defaults
remain nested 5 x 5 CV, the 41-alpha grid, and lambda.1se. Metrics are outer holdout
results, not final refit predictions or final calibration-fit statistics. No automatic
validation threshold is imposed; domain suitability and distribution rights require review.

Inspection traverses values, names, attributes, and nested component metadata. It flags
exact private IDs, occurrences of supplied training text/input paths, and common private
absolute paths. Exact ID matching avoids false substring hits from short numeric IDs.
Heuristics cannot identify every sensitive free-text detail. Inspect all retained labels,
prose, reasons, and provenance; resolve findings in inputs/metadata and rebuild in fresh
directories. Do not clear real findings merely to bypass review. Review reports themselves
can contain restricted material and must remain private.

For a reproducibility comparison, reuse the same timestamp, data, environment, and
settings with new empty output directories. Cross-platform byte identity is not promised.
For cached provenance with unknown versions, retain that limitation in the review and
public notes; do not claim verified extraction.

## Explicit source promotion

After reviewing the concrete staged artifact, its nested-CV evidence, domain suitability,
privacy, distribution rights, and size, record the five decisions in the private report:

```r
review$approved <- list(provenance = TRUE, license = TRUE, validation = TRUE,
                       privacy = TRUE, size = TRUE)
saveRDS(review, result$review_path)
promote_bundled_models(result$stage_dir, result$review_path,
                      source_dir = "/path/to/pairwiseLLM-source")
```

Promotion checks that the reviewed manifest has not changed, revalidates every candidate,
and rejects outstanding restricted-value findings. It preserves existing bundle entries;
name collisions require `overwrite = TRUE`. A complete replacement directory is verified
before swapping the source `inst/models` directory, with rollback on a failed swap.
This is an explicit source-tree edit, not package installation, user registration, a git
commit, or release publication. Run it with no concurrent edits to the bundle directory.
It requires a source checkout with DESCRIPTION and R sources, not an installed package.

Before release, inspect the combined bundle's size and rebuild/inspect the source archive.
Only README, manifest, and approved RDS files belong under `inst/models`. Training data,
cache files, full audits, review reports, and model/environment binaries stay outside.
The existing `.Rbuildignore` exclusions also keep `data-raw/` and Python bytecode caches out.

TODO after the series: configure approved datasets, train real models, review the actual
metrics/provenance/licenses, promote reviewed artifacts, and reassess source-package size.
Infrastructure completion does not mark that future model-build work complete.
