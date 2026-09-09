# Manifest version 1 is separate from model, ensemble, and metadata versions.
.warm_start_bundle_json <- function(x) {
  normalize <- function(value) {
    if (!is.null(names(value))) value <- as.list(value)
    if (is.list(value)) value <- lapply(value, normalize)
    value
  }
  as.character(jsonlite::toJSON(normalize(x), auto_unbox = TRUE, null = "null", na = "null",
    digits = NA, force = TRUE))
}

.warm_start_bundle_metadata <- function(model) {
  required <- c("name", "version", "domain", "license", "notes", "extraction_provenance")
  if (!all(required %in% names(model$metadata))) {
    rlang::abort("Bundled artifacts require name, version, domain, license, notes, and extraction_provenance.")
  }
  model$metadata[required]
}

# Build records exclusively from the actual artifact; JSON normalization retains NA as null.
.warm_start_bundle_record <- function(model, path, built_at) {
  .validate_warm_start_artifact(model)
  .validate_warm_start_metadata(list(prepared_at = built_at))
  metadata <- .warm_start_bundle_metadata(model)
  name <- .warm_start_model_name(metadata$name)
  if (!identical(name, metadata$name) || !identical(basename(path), paste0(name, ".rds"))) {
    rlang::abort("Bundled name must be canonical and match the artifact filename.")
  }
  ensemble <- inherits(model, "pairwiseLLM_warm_ensemble")
  components <- if (ensemble) model$components else stats::setNames(list(model), name)
  records <- lapply(seq_along(components), function(i) {
    component <- components[[i]]
    if (component$format_version != 2L || component$calibration$status != "oof_linear") {
      rlang::abort("Bundled components require calibrated summary-only models; retain full audits privately.")
    }
    list(component_name = names(components)[i], metadata = .warm_start_bundle_metadata(component),
      format_version = component$format_version, training = component$training,
      tuning = component$tuning, validation = component$validation)
  })
  list(name = name, filename = basename(path), metadata = metadata,
    artifact_type = if (ensemble) "ensemble" else "model", format_version = model$format_version,
    schema = model$schema, target = model$outcome$definition, built_at = built_at,
    size_bytes = unname(file.info(path)$size),
    checksum = list(algorithm = "md5", value = unname(tools::md5sum(path))), components = records)
}

.warm_start_bundle_manifest <- function(root) {
  path <- file.path(root, "manifest.json")
  if (!file.exists(path) || dir.exists(path)) rlang::abort(paste0("Cannot read bundled manifest: ", path))
  manifest <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) rlang::abort(paste0("Cannot read bundled manifest: ", path), parent = e))
  if (!is.list(manifest) || !identical(names(manifest), c("manifest_version", "artifacts")) ||
      !identical(manifest$manifest_version, 1L) || !is.list(manifest$artifacts) ||
      !is.null(names(manifest$artifacts))) {
    rlang::abort("Invalid or unsupported bundled manifest version/structure.")
  }
  names <- vapply(manifest$artifacts, function(entry) {
    if (!is.list(entry) || !.warm_start_string(entry$name) ||
        !identical(entry$name, .warm_start_model_name(entry$name)) ||
        !identical(entry$filename, paste0(entry$name, ".rds"))) {
      rlang::abort("Invalid bundled manifest name or filename.")
    }
    entry$name
  }, character(1))
  if (anyDuplicated(names)) rlang::abort("Duplicate bundled manifest names.")
  files <- list.files(root, pattern = "\\.rds$", all.files = TRUE)
  if (!setequal(files, vapply(manifest$artifacts, `[[`, character(1), "filename"))) {
    rlang::abort("Bundled manifest inventory mismatch: missing or unlisted artifact files.")
  }
  stats::setNames(manifest$artifacts, names)
}

.warm_start_bundle_model <- function(root, name, manifest = NULL) {
  if (is.null(manifest)) manifest <- .warm_start_bundle_manifest(root)
  entry <- manifest[[name]]
  if (is.null(entry)) rlang::abort(paste0("No bundled manifest entry for '", name, "'."))
  path <- .warm_start_registry_entry(root, name)
  if (!identical(entry$checksum$algorithm, "md5") ||
      !identical(entry$checksum$value, unname(tools::md5sum(path))) ||
      !isTRUE(entry$size_bytes == unname(file.info(path)$size))) {
    rlang::abort(paste0("Bundled checksum or size mismatch: ", path))
  }
  model <- .warm_start_read_model(path)
  expected <- .warm_start_bundle_record(model, path, entry$built_at)
  if (!identical(.warm_start_bundle_json(entry), .warm_start_bundle_json(expected))) {
    rlang::abort(paste0("Bundled manifest metadata does not match artifact: ", path))
  }
  model
}
