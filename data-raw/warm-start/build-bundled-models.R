# Maintainer-only workflow. Sourcing defines functions; it never reads data or builds models.
# See README.md for the explicit RDS configuration and review/promotion commands.

bundle_internal <- function(name) getFromNamespace(name, "pairwiseLLM")

bundle_assert <- function(ok, message) {
  if (!isTRUE(ok)) rlang::abort(message)
}

bundle_private_directory <- function(path) {
  bundle_assert(is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path),
    "Supply an explicit private output directory.")
  canonical <- bundle_internal(".warm_start_canonical_path")(path)
  package <- normalizePath(system.file(package = "pairwiseLLM"), winslash = "/")
  bundle_assert(!bundle_internal(".warm_start_within")(canonical, package),
    "Audit and staging directories must be outside the package tree.")
  if (!dir.exists(canonical)) {
    bundle_assert(dir.create(canonical, recursive = TRUE), "Cannot create private output directory.")
  }
  canonical
}

# All retained character values, field names, and attributes are included in the private report.
# Exact ID matching avoids treating a short numeric ID as a substring of every version/number.
bundle_inspect <- function(object, ids = character(), texts = character(), paths = character()) {
  leaves <- list()
  walk <- function(x, location) {
    if (is.character(x)) leaves[[length(leaves) + 1L]] <<- list(location = location, value = x)
    attrs <- attributes(x)
    if (length(attrs)) {
      walk(names(attrs), paste0(location, "@attribute_names"))
      for (i in seq_along(attrs)) walk(attrs[[i]], paste0(location, "@attributes[[", i, "]]"))
    }
    if (is.list(x)) {
      for (i in seq_along(x)) walk(x[[i]], paste0(location, "[[", i, "]]"))
    }
  }
  walk(object, "artifact")
  findings <- lapply(leaves, function(leaf) {
    values <- leaf$value
    restricted <- values %in% ids
    for (needle in unique(c(texts, paths))) {
      if (!is.na(needle) && nzchar(needle)) restricted <- restricted | grepl(needle, values, fixed = TRUE)
    }
    private_path <- grepl("(^|[[:space:]])(/(home|Users|tmp|mnt|private)/|[A-Za-z]:[\\\\/]|file://)", values)
    list(location = leaf$location, value = values[which(restricted | private_path)])
  })
  list(character_fields = leaves, findings = Filter(function(x) length(x$value) > 0L, findings))
}

bundle_numeric_predictions <- function(model, features) {
  p <- stats::predict(model, features)
  lapply(p[vapply(p, is.numeric, logical(1))], as.numeric)
}

build_bundled_models <- function(config_path) {
  config <- readRDS(config_path)
  bundle_assert(is.list(config) && is.list(config$tasks) && length(config$tasks) >= 1L,
    "Configuration requires a named nonempty tasks list.")
  task_names <- names(config$tasks)
  bundle_assert(!is.null(task_names) && !anyDuplicated(task_names) && all(nzchar(trimws(task_names))),
    "Task component names must be unique and nonblank.")
  bundle_internal(".validate_warm_start_metadata")(list(prepared_at = config$built_at))
  bundle_assert(!is.null(config$built_at), "Supply a fixed UTC built_at timestamp.")
  stage <- bundle_private_directory(config$stage_dir)
  audit <- bundle_private_directory(config$audit_dir)
  bundle_assert(!bundle_internal(".warm_start_within")(stage, audit) &&
      !bundle_internal(".warm_start_within")(audit, stage), "Audit and staging directories must be separate.")
  bundle_assert(!length(list.files(stage, all.files = TRUE, no.. = TRUE)), "Staging directory must be empty.")
  bundle_assert(!length(list.files(audit, all.files = TRUE, no.. = TRUE)), "Audit directory must be empty.")
  components <- list()
  datasets <- list()
  private_ids <- private_texts <- character()
  private_paths <- c(normalizePath(config_path, winslash = "/"), stage, audit)
  for (i in seq_along(config$tasks)) {
    task <- config$tasks[[i]]
    data <- readRDS(task$data_path)
    bundle_assert(is.list(data) && !is.null(data$ids) && !is.null(data$theta) &&
        xor(is.null(data$texts), is.null(data$features)),
      "Each dataset needs ids, theta, and exactly one of texts/features.")
    private_ids <- c(private_ids, as.character(data$ids))
    private_texts <- c(private_texts, data$texts)
    private_paths <- c(private_paths, task$data_path, normalizePath(task$data_path, winslash = "/"), task$python)
    if (!is.null(data$texts)) {
      # Capture evidence in this extraction run, not retrospectively for cached features.
      status <- pairwiseLLM::warm_start_python_status(python = task$python)
      bundle_assert(status$available, "Configured extraction environment is unavailable.")
      features <- pairwiseLLM::extract_warm_start_features(data$ids, data$texts, python = task$python)
      observed <- unlist(status$observed)
      provenance <- c(status = "captured_during_extraction",
        stats::setNames(as.character(observed), names(observed)),
        environment_md5 = unname(tools::md5sum(system.file("python", "audit-environment.json",
          package = "pairwiseLLM"))),
        lock_md5 = unname(tools::md5sum(system.file("python", "requirements-warm-start.lock",
          package = "pairwiseLLM"))))
      # Full environment/resource evidence is private; portable versions exclude interpreter paths.
      saveRDS(list(status = status, audit_environment = jsonlite::read_json(
        system.file("python", "audit-environment.json", package = "pairwiseLLM"))),
        file.path(audit, paste0("extraction-", i, ".rds")))
      saveRDS(list(features = features, extraction_provenance = provenance),
        file.path(audit, paste0("features-", i, ".rds")))
    } else {
      features <- data$features
      provenance <- data$extraction_provenance
      if (is.null(provenance)) provenance <- c(status = "unavailable")
    }
    tuning <- task$tuning
    if (is.null(tuning)) tuning <- list()
    bundle_assert(is.list(tuning) && (length(tuning) == 0L ||
        (!is.null(names(tuning)) && !anyDuplicated(names(tuning)) &&
          all(names(tuning) %in% c("seed", "outer_folds", "inner_folds", "alpha_grid", "lambda_rule")))),
      "Tuning accepts only seed, folds, alpha_grid, and lambda_rule.")
    model <- do.call(pairwiseLLM::fit_warm_start_model, c(list(ids = data$ids, theta = data$theta,
      task_id = task$task_id, features = features), tuning))
    metadata <- task$metadata
    metadata$extraction_provenance <- provenance
    metadata$prepared_at <- config$built_at
    model <- pairwiseLLM::prepare_warm_start_model(model, metadata)
    pairwiseLLM::save_warm_start_model(model, file.path(audit, paste0("component-", i, ".rds")))
    components[[task_names[i]]] <- model
    datasets[[i]] <- features
  }
  model <- if (length(components) == 1L) components[[1]] else
    do.call(pairwiseLLM::ensemble_warm_start_models, components)
  metadata <- config$metadata
  metadata$extraction_provenance <- if (length(components) == 1L) {
    components[[1]]$metadata$extraction_provenance
  } else {
    c(status = "see_components")
  }
  metadata$prepared_at <- config$built_at
  full <- pairwiseLLM::prepare_warm_start_model(model, metadata)
  pairwiseLLM::save_warm_start_model(full, file.path(audit, "full-artifact.rds"))
  reduced <- pairwiseLLM::prepare_warm_start_model(full, omit_audit = TRUE)
  for (features in datasets) {
    bundle_assert(identical(bundle_numeric_predictions(full, features),
      bundle_numeric_predictions(reduced, features)), "Audit reduction changed numeric predictions.")
  }
  name <- bundle_internal(".warm_start_model_name")(reduced$metadata$name)
  path <- file.path(stage, paste0(name, ".rds"))
  pairwiseLLM::save_warm_start_model(reduced, path)
  restored <- pairwiseLLM::load_warm_start_model(path)
  for (features in datasets) {
    bundle_assert(identical(stats::predict(reduced, features), stats::predict(restored, features)),
      "Saved candidate changed predictions.")
  }
  record <- bundle_internal(".warm_start_bundle_record")(restored, path, config$built_at)
  manifest <- list(manifest_version = 1L, artifacts = list(record))
  manifest_path <- file.path(stage, "manifest.json")
  writeLines(bundle_internal(".warm_start_bundle_json")(manifest), manifest_path)
  bundle_internal(".warm_start_bundle_model")(stage, name)
  inspection <- bundle_inspect(list(model = restored, manifest = manifest),
    private_ids, private_texts, private_paths)
  report <- list(manifest_md5 = unname(tools::md5sum(manifest_path)),
    artifact = record, inspection = inspection, total_bytes = unname(file.info(path)$size),
    approved = list(provenance = FALSE, license = FALSE, validation = FALSE, privacy = FALSE, size = FALSE))
  review_path <- file.path(audit, "publication-review.rds")
  saveRDS(report, review_path)
  message("Candidate staged. Review nested metadata, metrics, rights, privacy, and size in: ", review_path)
  invisible(list(stage_dir = stage, review_path = review_path))
}

# Promotion operates on source inst/models only. It never writes an installed package or user registry.
promote_bundled_models <- function(stage_dir, review_path, source_dir, overwrite = FALSE) {
  bundle_assert(identical(overwrite, TRUE) || identical(overwrite, FALSE), "overwrite must be TRUE or FALSE.")
  stage <- normalizePath(stage_dir, winslash = "/", mustWork = TRUE)
  source <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  bundle_assert(file.exists(file.path(source, "DESCRIPTION")) && dir.exists(file.path(source, "R")) &&
      identical(unname(read.dcf(file.path(source, "DESCRIPTION"), fields = "Package")[1, 1]), "pairwiseLLM"),
    "source_dir must be the pairwiseLLM source tree.")
  root <- file.path(source, "inst", "models")
  bundle_assert(!bundle_internal(".warm_start_within")(stage, source), "Staging must be outside the source tree.")
  report <- readRDS(review_path)
  bundle_assert(identical(report$manifest_md5, unname(tools::md5sum(file.path(stage, "manifest.json")))),
    "Reviewed manifest checksum changed; rebuild/review the candidate.")
  decisions <- c("provenance", "license", "validation", "privacy", "size")
  bundle_assert(identical(names(report$approved), decisions) &&
      all(vapply(report$approved, identical, logical(1), TRUE)), "Publication review is not approved.")
  bundle_assert(is.list(report$inspection) && is.list(report$inspection$findings) &&
      !length(report$inspection$findings), "Resolve restricted identifiers or paths and rebuild before promotion.")
  incoming <- bundle_internal(".warm_start_bundle_manifest")(stage)
  bundle_assert(length(incoming) > 0L, "No staged artifacts to promote.")
  existing <- bundle_internal(".warm_start_bundle_manifest")(root)
  allowed <- c("README.md", "manifest.json", vapply(existing, `[[`, character(1), "filename"))
  bundle_assert(!length(setdiff(list.files(root, all.files = TRUE, no.. = TRUE), allowed)),
    "Unexpected files in source bundle directory; keep caches and training outputs private.")
  for (name in names(existing)) bundle_internal(".warm_start_bundle_model")(root, name, existing)
  for (name in names(incoming)) bundle_internal(".warm_start_bundle_model")(stage, name, incoming)
  bundle_assert(overwrite || !length(intersect(names(existing), names(incoming))),
    "Bundle already exists; use overwrite = TRUE after review.")
  # Assemble and verify a complete replacement directory before touching the source bundle.
  replacement <- tempfile(".models-stage-", tmpdir = dirname(root))
  backup <- tempfile(".models-backup-", tmpdir = dirname(root))
  bundle_assert(dir.create(replacement), "Cannot create publication staging directory.")
  on.exit(unlink(replacement, recursive = TRUE), add = TRUE)
  files <- list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  bundle_assert(all(file.copy(files, replacement, recursive = TRUE)), "Cannot stage existing bundle directory.")
  for (name in names(incoming)) {
    bundle_assert(file.copy(file.path(stage, incoming[[name]]$filename), replacement, overwrite = TRUE),
      "Cannot stage candidate artifact.")
  }
  existing[names(incoming)] <- incoming
  merged <- list(manifest_version = 1L, artifacts = unname(existing[order(names(existing))]))
  writeLines(bundle_internal(".warm_start_bundle_json")(merged), file.path(replacement, "manifest.json"))
  for (name in names(existing)) bundle_internal(".warm_start_bundle_model")(replacement, name)
  bundle_assert(file.rename(root, backup), "Cannot move source bundle to rollback directory.")
  if (!file.rename(replacement, root)) {
    restored <- file.rename(backup, root)
    rlang::abort(paste("Cannot publish bundle; rollback restored:", restored, "Backup:", backup))
  }
  unlink(backup, recursive = TRUE)
  invisible(root)
}
