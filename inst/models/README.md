# Bundled warm-start model location

This directory is reserved for read-only `<normalized-name>.rds` artifacts loaded
through `load_warm_start_model(name = ..., source = "bundled")`. No substantive
pretrained model is shipped by Task 05. User models belong under
`file.path(tools::R_user_dir("pairwiseLLM", "data"), "models")`, never here.

Both sources use the public model class, validator, compressed RDS storage and
prediction path. Use `prepare_warm_start_model(..., omit_audit = TRUE)` explicitly
before distributing an artifact without row-level evidence. Review remaining
labels, notes and provenance for restricted information. Reduced summaries cannot
be recomputed without the separately retained full audit artifact.

Task 07 adds the maintainer build workflow and versioned manifest/checksums,
reviews redistribution rights and compressed artifact/package size, and uses the
public APIs. It must not present synthetic fixtures as validated production models.
