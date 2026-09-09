test_that("empty installed manifest ships no predictive fixtures", {
  python_loaded <- "reticulate" %in% loadedNamespaces()
  root <- system.file("models", package = "pairwiseLLM")
  expect_length(.warm_start_bundle_manifest(root), 0)
  expect_equal(nrow(list_warm_start_models("bundled")), 0)
  expect_error(.warm_start_bundle_model(root, "absent"), "No bundled manifest entry")
  expect_identical("reticulate" %in% loadedNamespaces(), python_loaded)
})

test_that("bundle verification shares single and ensemble deployment paths", {
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir())
  user_root <- .warm_start_registry_root("user")
  local_mocked_bindings(.warm_start_registry_root = function(source) {
    if (source == "user") user_root else root
  })
  single <- warm_bundle_model()
  ensemble <- prepare_warm_start_model(ensemble_warm_start_models(
    first = single, second = warm_bundle_model("other")), warm_bundle_metadata("ensemble-test"))
  for (model in list(single, ensemble)) {
    unlink(list.files(root, full.names = TRUE))
    manifest <- warm_bundle_write(root, model)
    name <- model$metadata$name
    expect_identical(.warm_start_bundle_model(root, name), model)
    expect_identical(load_warm_start_model(name = name, source = "bundled"), model)
    register_warm_start_model(model, name)
    expect_identical(predict(load_warm_start_model(name = name, source = "user"), warm_core_features()),
      predict(load_warm_start_model(name = name, source = "bundled"), warm_core_features()))
    expect_error(load_warm_start_model(name = name), "Ambiguous")
    expect_equal(nrow(list_warm_start_models("bundled")), 1)
    expect_identical(.warm_start_bundle_json(manifest),
      .warm_start_bundle_json(jsonlite::read_json(file.path(root, "manifest.json"))))
    expect_lt(file.info(file.path(root, paste0(name, ".rds")))$size, 20000)
  }
})

test_that("manifest structure, inventory, checksums, and metadata fail closed", {
  root <- withr::local_tempdir()
  path <- file.path(root, "manifest.json")
  expect_error(.warm_start_bundle_manifest(root), "Cannot read")
  writeLines("broken", path)
  expect_error(.warm_start_bundle_manifest(root), "Cannot read")
  good <- warm_bundle_write(root)
  for (bad in list(NULL, list(), list(manifest_version = 2L, artifacts = list()),
      list(manifest_version = 1L, artifacts = list(named = good$artifacts[[1]])))) {
    writeLines(.warm_start_bundle_json(bad), path)
    expect_error(.warm_start_bundle_manifest(root), "version/structure")
  }
  mutate <- function(change, pattern) {
    bad <- change(good)
    writeLines(.warm_start_bundle_json(bad), path)
    expect_error(.warm_start_bundle_model(root, "synthetic-test"), pattern)
  }
  mutate(function(x) {
    x$artifacts[[1]]$filename <- "../escape.rds"
    x
  }, "filename")
  mutate(function(x) {
    x$artifacts[[1]]$name <- "UPPER"
    x
  }, "name")
  mutate(function(x) {
    x$artifacts <- rep(x$artifacts, 2)
    x
  }, "Duplicate")
  mutate(function(x) {
    x$artifacts <- list()
    x
  }, "inventory")
  mutate(function(x) {
    x$artifacts[[1]]$checksum$value <- "bad"
    x
  }, "checksum")
  mutate(function(x) {
    x$artifacts[[1]]$checksum$algorithm <- "sha256"
    x
  }, "checksum")
  mutate(function(x) {
    x$artifacts[[1]]$size_bytes <- 0
    x
  }, "size")
  mutate(function(x) {
    x$artifacts[[1]]$schema <- "future"
    x
  }, "metadata")
  mutate(function(x) {
    x$artifacts[[1]]$built_at <- "yesterday"
    x
  }, "timestamp")
  mutate(function(x) {
    x$artifacts[[1]]$components[[1]]$training$n <- 999
    x
  }, "metadata")
  mutate(function(x) {
    x$artifacts[[1]]$metadata$version <- "other"
    x
  }, "metadata")
  writeLines(.warm_start_bundle_json(good), path)
  file <- file.path(root, "synthetic-test.rds")
  unlink(file)
  expect_error(.warm_start_bundle_manifest(root), "inventory")
  dir.create(file)
  expect_error(.warm_start_bundle_model(root, "synthetic-test"), "Invalid")
  unlink(file, recursive = TRUE)
  writeLines("corrupt", file)
  expect_error(.warm_start_bundle_model(root, "synthetic-test"), "checksum")
})

test_that("bundle records require calibrated reduced models and publication metadata", {
  root <- withr::local_tempdir()
  model <- warm_bundle_model()
  path <- file.path(root, "synthetic-test.rds")
  save_warm_start_model(model, path)
  record <- function(x, file = path) .warm_start_bundle_record(x, file, "2026-09-08T00:00:00Z")
  expect_error(record(warm_core_model()), "require name")
  core <- prepare_warm_start_model(warm_core_model(), warm_bundle_metadata(), omit_audit = TRUE)
  expect_error(record(core), "calibrated summary-only")
  expect_error(record(model, file.path(root, "other.rds")), "filename")
  model$metadata$name <- "UPPER"
  expect_error(record(model), "canonical")
  model <- warm_bundle_model()
  model$validation$metrics$pearson_r <- NA_real_
  model$validation$metrics$squared_pearson_r <- NA_real_
  model$validation$metrics$undefined_reasons <- "Synthetic undefined diagnostic."
  warm_bundle_write(root, model)
  expect_identical(.warm_start_bundle_model(root, "synthetic-test"), model)
  skip_on_os("windows")
  outside <- withr::local_tempfile()
  file.copy(path, outside)
  unlink(path)
  expect_true(file.symlink(outside, path))
  expect_error(.warm_start_bundle_model(root, "synthetic-test"), "escaping")
})
