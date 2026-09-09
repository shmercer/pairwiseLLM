warm_bundle_workflow <- function() {
  script <- test_path("..", "..", "data-raw", "warm-start", "build-bundled-models.R")
  skip_if_not(file.exists(script), "Maintainer workflow is excluded from installed/source packages.")
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  env
}

warm_bundle_config <- function(root, count = 2L) {
  features <- warm_core_features(15)
  features$item_id <- paste0("private-student-", seq_len(nrow(features)))
  dataset <- file.path(root, "private-data.rds")
  saveRDS(list(ids = features$item_id, theta = warm_core_theta(features), features = features,
    extraction_provenance = c(status = "synthetic_test_only")), dataset)
  tasks <- lapply(seq_len(count), function(i) {
    list(data_path = dataset,
    task_id = paste0("synthetic-task-", i), metadata = warm_bundle_metadata(paste0("component-", i)),
      tuning = list(alpha_grid = c(0, 1)))
  })
  names(tasks) <- paste0("task-", seq_len(count))
  config <- list(tasks = tasks, built_at = "2026-09-08T00:00:00Z",
    metadata = warm_bundle_metadata("workflow-test"),
    stage_dir = file.path(root, "stage"), audit_dir = file.path(root, "audit"))
  path <- file.path(root, "config.rds")
  saveRDS(config, path)
  list(path = path, config = config, features = features)
}

test_that("recursive publication inspection includes names, values, attributes, and nested components", {
  w <- warm_bundle_workflow()
  x <- list(components = list(first = list(notes = "private-student-1",
    reason = "See /home/private/corpus", provenance = c(source = "C:\\Users\\secret"))))
  attr(x, "private-label") <- "Confidential training text"
  x[["private-student-2"]] <- "safe"
  result <- w$bundle_inspect(x, ids = c("private-student-1", "private-student-2"),
    texts = "Confidential training text")
  expect_gte(length(result$findings), 5)
  expect_true(any(vapply(result$character_fields, function(x) grepl("attributes", x$location), logical(1))))
  expect_length(w$bundle_inspect(list(version = "1.3.1"), ids = "1")$findings, 0)
  expect_length(w$bundle_inspect(list(source = "prefix PRIVATE suffix"), paths = "PRIVATE")$findings, 1)
  expect_error(w$bundle_private_directory(system.file(package = "pairwiseLLM")), "outside")
  expect_error(w$bundle_private_directory(NULL), "explicit")
})

test_that("public build, deterministic staging, review, promotion, and replacement work", {
  python_loaded <- "reticulate" %in% loadedNamespaces()
  skip_if_not_installed("glmnet")
  w <- warm_bundle_workflow()
  root <- withr::local_tempdir()
  f <- warm_bundle_config(root)
  expect_message(result <- w$build_bundled_models(f$path), "Candidate staged")
  report <- readRDS(result$review_path)
  expect_length(report$inspection$findings, 0)
  expect_false(any(unlist(report$approved)))
  model <- load_warm_start_model(file.path(result$stage_dir, "workflow-test.rds"))
  full <- load_warm_start_model(file.path(f$config$audit_dir, "full-artifact.rds"))
  expect_identical(model$components[[1]]$format_version, 2L)
  expect_identical(full$components[[1]]$format_version, 1L)
  expect_identical(w$bundle_numeric_predictions(model, f$features), w$bundle_numeric_predictions(full, f$features))
  expect_identical(report$artifact$components[[1]]$validation$metrics, full$components[[1]]$validation$metrics)
  expect_lt(report$total_bytes, 30000)
  expect_error(w$build_bundled_models(f$path), "must be empty")
  # Fixed timestamps/configuration and task-specific seeds reproduce serialized candidates.
  config <- f$config
  config$stage_dir <- file.path(root, "stage-repeat")
  config$audit_dir <- file.path(root, "audit-repeat")
  saveRDS(config, f$path)
  expect_message(repeated <- w$build_bundled_models(f$path), "Candidate staged")
  expect_identical(unname(tools::md5sum(file.path(repeated$stage_dir, "workflow-test.rds"))),
    unname(tools::md5sum(file.path(result$stage_dir, "workflow-test.rds"))))
  source <- file.path(root, "source")
  dir.create(file.path(source, "inst", "models"), recursive = TRUE)
  dir.create(file.path(source, "R"))
  writeLines("Package: pairwiseLLM", file.path(source, "DESCRIPTION"))
  writeLines('{"manifest_version":1,"artifacts":[]}', file.path(source, "inst", "models", "manifest.json"))
  promote <- function(overwrite = FALSE) {
    w$promote_bundled_models(result$stage_dir, result$review_path, source, overwrite)
  }
  expect_error(promote(), "not approved")
  report$approved[] <- list(TRUE)
  report$inspection$findings <- list(list(value = "private"))
  saveRDS(report, result$review_path)
  expect_error(promote(), "restricted")
  report$inspection$findings <- list()
  saveRDS(report, result$review_path)
  destination <- promote()
  expect_identical(.warm_start_bundle_model(destination, "workflow-test"), model)
  expect_error(promote(), "already exists")
  expect_identical(promote(TRUE), destination)
  before <- tools::md5sum(file.path(destination, "workflow-test.rds"))
  w$file.rename <- function(from, to) {
    if (grepl(".models-stage-", from, fixed = TRUE)) return(FALSE)
    base::file.rename(from, to)
  }
  expect_error(promote(TRUE), "rollback restored: TRUE")
  expect_identical(tools::md5sum(file.path(destination, "workflow-test.rds")), before)
  rm("file.rename", envir = w)
  w$file.copy <- function(...) FALSE
  expect_error(promote(TRUE), "Cannot stage existing")
  rm("file.copy", envir = w)
  expect_identical(.warm_start_bundle_model(destination, "workflow-test"), model)
  writeLines("cache", file.path(destination, "cache.txt"))
  expect_error(promote(TRUE), "Unexpected files")
  unlink(file.path(destination, "cache.txt"))
  expect_error(promote(NA), "TRUE or FALSE")
  expect_error(w$promote_bundled_models(result$stage_dir, result$review_path, root), "source tree")
  report$manifest_md5 <- "changed"
  saveRDS(report, result$review_path)
  expect_error(promote(TRUE), "checksum changed")
  expect_identical("reticulate" %in% loadedNamespaces(), python_loaded)
})

test_that("workflow validates configuration and preserves unknown cached provenance", {
  skip_if_not_installed("glmnet")
  w <- warm_bundle_workflow()
  root <- withr::local_tempdir()
  f <- warm_bundle_config(root, 1L)
  dataset <- readRDS(f$config$tasks[[1]]$data_path)
  dataset$extraction_provenance <- NULL
  saveRDS(dataset, f$config$tasks[[1]]$data_path)
  expect_message(result <- w$build_bundled_models(f$path), "Candidate staged")
  model <- load_warm_start_model(file.path(result$stage_dir, "workflow-test.rds"))
  # Artifact metadata cannot override actual/unknown feature provenance.
  component <- load_warm_start_model(file.path(f$config$audit_dir, "component-1.rds"))
  expect_identical(component$metadata$extraction_provenance, c(status = "unavailable"))
  expect_s3_class(model, "pairwiseLLM_warm_model")
  expect_identical(model$metadata$extraction_provenance, c(status = "unavailable"))
  for (bad in list(NULL, list(tasks = list()), list(tasks = list(list())))) {
    saveRDS(bad, f$path)
    expect_error(w$build_bundled_models(f$path), "tasks|names")
  }
  config <- f$config
  config$stage_dir <- file.path(root, "new-stage")
  config$audit_dir <- file.path(root, "new-audit")
  config$tasks[[1]]$tuning <- list(unknown = TRUE)
  saveRDS(config, f$path)
  expect_error(w$build_bundled_models(f$path), "Tuning accepts")
})

test_that("configured text builds capture extraction evidence in the same run", {
  python <- Sys.getenv("PAIRWISELLM_TEST_PYTHON", "")
  skip_if(!nzchar(python) || !file.exists(python), "Set PAIRWISELLM_TEST_PYTHON for real extraction.")
  skip_if_not_installed("reticulate")
  skip_if_not_installed("glmnet")
  w <- warm_bundle_workflow()
  root <- withr::local_tempdir()
  f <- warm_bundle_config(root, 1L)
  texts <- vapply(seq_len(15), function(n) {
    paste(rep("Students explain their reasoning and support the argument with evidence.", n), collapse = " ")
  }, character(1))
  task <- f$config$tasks[[1]]
  saveRDS(list(ids = f$features$item_id, theta = seq_len(15), texts = texts), task$data_path)
  f$config$tasks[[1]]$python <- python
  f$config$tasks[[1]]$tuning <- list(alpha_grid = 0, lambda_rule = "lambda.min")
  saveRDS(f$config, f$path)
  expect_message(result <- withCallingHandlers(w$build_bundled_models(f$path), warning = function(w) {
    if (startsWith(conditionMessage(w), "Importing 'parser.split_arg_string' is deprecated")) {
      invokeRestart("muffleWarning")
    }
  }), "Candidate staged")
  model <- load_warm_start_model(file.path(result$stage_dir, "workflow-test.rds"))
  provenance <- model$metadata$extraction_provenance
  expect_identical(provenance[["status"]], "captured_during_extraction")
  expect_true(all(c("python", "textdescriptives", "textstat", "environment_md5", "lock_md5") %in% names(provenance)))
  expect_false(python %in% provenance)
  expect_true(file.exists(file.path(f$config$audit_dir, "extraction-1.rds")))
  cached <- readRDS(file.path(f$config$audit_dir, "features-1.rds"))
  expect_identical(attr(cached$features, "warm_start_schema"), "writing_features_v1")
  expect_identical(cached$extraction_provenance, provenance)
  expect_length(readRDS(result$review_path)$inspection$findings, 0)
})
