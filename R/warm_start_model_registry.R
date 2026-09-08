#' Register, inspect, or remove user warm-start models
#'
#' @param model A valid [pairwiseLLM_warm_model].
#' @param name Model registry name, separate from the assessment task ID.
#' @param overwrite Explicitly replace an existing user entry. Default FALSE.
#' @param source Which registries to list.
#' @return Registration/removal invisibly return the entry path. Listing returns
#'   a tibble with name, source, path, version, format_version, schema, target, n,
#'   calibration, audit_status, size_bytes, metadata, and validation. Metadata and
#'   validation are list columns; unspecified metadata versions are NA character values.
#' @details
#' User models live in the `models` subdirectory of
#' `tools::R_user_dir("pairwiseLLM", "data")`. Only explicit registration creates
#' this directory. Names are trimmed, ASCII-lowercased, and spaces/underscores
#' become hyphens. The result must contain alphanumeric segments separated by
#' single hyphens. Dots, path separators, traversal, and escaping symlinks are
#' rejected. The normalized name determines collisions and the `<name>.rds` file.
#'
#' Installed bundled models are read-only `models/<name>.rds` resources. Both
#' sources use the same model validator and prediction method. Listing reads and
#' validates artifacts without glmnet or Python; corrupt entries produce errors
#' naming their paths. Missing registries return empty results and are not created.
#'
#' Registration preserves full audit evidence unless explicitly reduced beforehand.
#' Compressed files replace entries only with explicit overwrite; no backup history
#' accumulates. Use listing and removal to manage obsolete user models. Models are
#' not automatically removed based on age. Removal never affects bundled models.
#' Tests and examples must redirect `R_USER_DATA_DIR` to a temporary directory.
#' No user models are written into the installed package tree.
#' @export
register_warm_start_model <- function(model, name, overwrite = FALSE) {
  .validate_warm_start_model(model)
  .warm_start_flag(overwrite, "overwrite")
  name <- .warm_start_model_name(name)
  root <- .warm_start_registry_root("user")
  .warm_start_user_root_safe(root)
  if (!dir.exists(root) && !dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
    rlang::abort(paste0("Cannot create user model registry: ", root))
  }
  path <- .warm_start_registry_entry(root, name)
  save_warm_start_model(model, path, overwrite)
}

#' @rdname register_warm_start_model
#' @export
remove_warm_start_model <- function(name) {
  root <- .warm_start_registry_root("user")
  .warm_start_user_root_safe(root)
  path <- .warm_start_registry_entry(root, .warm_start_model_name(name))
  if (!file.exists(path) || dir.exists(path)) rlang::abort("User model does not exist.")
  if (.warm_start_remove_file(path) != 0L || file.exists(path)) {
    rlang::abort("Cannot remove user model.")
  }
  invisible(path)
}

.warm_start_remove_file <- function(path) unlink(path)

.warm_start_registry_root <- function(source) {
  if (source == "user") return(file.path(tools::R_user_dir("pairwiseLLM", "data"), "models"))
  system.file("models", package = "pairwiseLLM")
}

.warm_start_model_name <- function(name) {
  if (!.warm_start_string(name)) rlang::abort("Model name must be one nonblank character string.")
  name <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", trimws(name))
  name <- gsub("[ _]+", "-", name)
  if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", name)) {
    rlang::abort("Invalid model name: use alphanumeric segments separated by hyphens, without paths or dots.")
  }
  name
}

# Resolve existing ancestors too, so a redirected data root cannot enter the package tree.
.warm_start_canonical_path <- function(path) {
  path <- path.expand(path)
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
  parent <- dirname(path)
  if (identical(parent, path)) rlang::abort("Cannot resolve registry path.")
  file.path(.warm_start_canonical_path(parent), basename(path))
}

.warm_start_within <- function(path, root) {
  if (.Platform$OS.type == "windows") {
    path <- tolower(path)
    root <- tolower(root)
  }
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

.warm_start_user_root_safe <- function(root) {
  installed <- system.file(package = "pairwiseLLM")
  if (nzchar(installed) && .warm_start_within(.warm_start_canonical_path(root),
      normalizePath(installed, winslash = "/", mustWork = TRUE))) {
    rlang::abort("User registry must not be inside the installed package tree; change R_USER_DATA_DIR.")
  }
}

.warm_start_registry_entry <- function(root, name) {
  if (!nzchar(root)) return("")
  path <- file.path(root, paste0(name, ".rds"))
  if (.warm_start_path_exists(path)) {
    if (!file.exists(path) || dir.exists(path) ||
        !.warm_start_within(normalizePath(path, winslash = "/", mustWork = TRUE),
          normalizePath(root, winslash = "/", mustWork = TRUE))) {
      rlang::abort(paste0("Invalid or escaping model registry entry: ", path))
    }
  }
  path
}

.warm_start_resolve_name <- function(name, source) {
  name <- .warm_start_model_name(name)
  sources <- if (source == "auto") c("user", "bundled") else source
  paths <- vapply(sources, function(s) .warm_start_registry_entry(.warm_start_registry_root(s), name), character(1))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) rlang::abort(paste0("No registered or bundled model named '", name, "'."))
  if (length(paths) > 1L) rlang::abort("Ambiguous model name; specify source = 'user' or 'bundled'.")
  unname(paths[[1]])
}

.warm_start_model_metadata <- function(path, name, source) {
  model <- .warm_start_read_model(path)
  metadata <- model$metadata
  version <- if (is.null(metadata$version)) NA_character_ else metadata$version
  tibble::tibble(name = name, source = source, path = path, version = version,
    format_version = model$format_version, schema = model$schema, target = model$outcome$definition,
    n = model$training$n, calibration = model$calibration$status,
    audit_status = if (model$format_version == 2L) "summary_only" else "full",
    size_bytes = unname(file.info(path)$size), metadata = list(metadata),
    validation = list(model$validation$metrics))
}

#' @rdname register_warm_start_model
#' @export
list_warm_start_models <- function(source = c("all", "user", "bundled")) {
  source <- match.arg(source)
  sources <- if (source == "all") c("user", "bundled") else source
  out <- tibble::tibble(name = character(), source = character(), path = character(),
    version = character(), format_version = integer(), schema = character(), target = character(),
    n = integer(), calibration = character(), audit_status = character(), size_bytes = double(),
    metadata = list(), validation = list())
  for (s in sources) {
    root <- .warm_start_registry_root(s)
    if (!nzchar(root) || !dir.exists(root)) next
    files <- sort(list.files(root, pattern = "\\.rds$", all.files = TRUE))
    for (file in files) {
      name <- sub("\\.rds$", "", file)
      if (!identical(name, .warm_start_model_name(name))) rlang::abort("Noncanonical model registry filename.")
      path <- .warm_start_registry_entry(root, name)
      out <- rbind(out, .warm_start_model_metadata(path, name, s))
    }
  }
  out[order(out$name, out$source), ]
}
