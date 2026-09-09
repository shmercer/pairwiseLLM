# Synthetic contract fixture only: these summary metrics are not performance evidence.
# The maintainer workflow tests separately exercise real public fitting on synthetic inputs.
warm_bundle_metadata <- function(name = "synthetic-test") {
  list(name = name, version = "test-1", domain = "Synthetic tests only", license = "CC0-1.0",
    notes = "Synthetic contract fixture; not a validated predictor.",
    extraction_provenance = c(status = "synthetic_test_only"), prepared_at = "2026-09-08T00:00:00Z")
}

warm_bundle_model <- function(name = "synthetic-test") {
  model <- prepare_warm_start_model(warm_core_model(), warm_bundle_metadata(name), omit_audit = TRUE)
  model$calibration <- pairwiseLLM:::.warm_start_calibration_fit(seq_len(30), seq_len(30))
  model$tuning <- list(seed = 1L, alpha_grid = c(0, 0.5, 1), lambda_rule = "lambda.1se",
    conventions = pairwiseLLM:::.warm_start_tuning_conventions())
  model$validation <- list(method = "nested_cv", outer_folds = 5L, inner_folds = 5L,
    metrics = list(pearson_r = 0.5, squared_pearson_r = 0.25, spearman_rho = 0.5,
      rmse = 1, mae = 0.5, calibration_intercept = 0, calibration_slope = 1,
      undefined_reasons = character()), warning_count = 0L)
  pairwiseLLM:::.validate_warm_start_artifact(model)
  model
}

warm_bundle_write <- function(root, model = warm_bundle_model()) {
  path <- file.path(root, paste0(model$metadata$name, ".rds"))
  save_warm_start_model(model, path, overwrite = TRUE)
  record <- pairwiseLLM:::.warm_start_bundle_record(model, path, "2026-09-08T00:00:00Z")
  manifest <- list(manifest_version = 1L, artifacts = list(record))
  writeLines(pairwiseLLM:::.warm_start_bundle_json(manifest), file.path(root, "manifest.json"))
  manifest
}
