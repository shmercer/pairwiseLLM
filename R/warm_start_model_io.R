#' Save or load a portable warm-start model
#'
#' @param model A valid [pairwiseLLM_warm_model] or [ensemble_warm_start_models()] ensemble.
#' @param path Explicit file path. Its parent must already exist when saving.
#' @param overwrite Allow replacement of an existing artifact. Default FALSE.
#' @param name Registered model name, mutually exclusive with `path`.
#' @param source Registry to search. `auto` errors if user and bundled names collide.
#' @return Saving invisibly returns the normalized destination path. Loading returns
#'   the validated model, unchanged from its serialized representation.
#' @details
#' Artifacts are compressed RDS objects, without an envelope or serialized glmnet
#' engine. Format versions 1 (full audit) and 2 (explicit summary-only) are supported,
#' independently of package version. Ensembles use their own format 1 and may
#' contain either supported single-model format. Use [prepare_warm_start_model()] to add
#' metadata or explicitly omit audit records before saving. Saving never strips
#' records or adds timestamps. Neither loading nor prediction from precomputed
#' features needs glmnet or Python.
#'
#' Exactly one of `path` or `name` is required for loading. Positional input means
#' a path; a missing file never falls back to a registry search. Explicit paths
#' require `source = "auto"`. Registry names follow [register_warm_start_model()].
#' Named bundled lookup verifies the installed manifest and artifact checksum.
#' Explicit file paths use ordinary artifact validation without a manifest.
#' Load only trusted RDS files; contract validation is not a serialization sandbox.
#'
#' Writes are staged in the destination directory and validated before publishing.
#' Failed writes clean up staging files. Replacement uses filesystem rename; if
#' the platform cannot replace an existing file this way, the operation fails and
#' leaves that file intact. No persistent backup history is created.
#' @examples
#' # A model already fitted from one assessment can be saved explicitly:
#' if (FALSE) {
#'   path <- tempfile(fileext = ".rds")
#'   save_warm_start_model(model, path)
#'   restored <- load_warm_start_model(path)
#'   unlink(path)
#' }
#' @export
save_warm_start_model <- function(model, path, overwrite = FALSE) {
  .validate_warm_start_artifact(model)
  .warm_start_flag(overwrite, "overwrite")
  path <- .warm_start_file_path(path)
  if (!dir.exists(dirname(path))) rlang::abort("The artifact parent directory must already exist.")
  path <- file.path(normalizePath(dirname(path), winslash = "/", mustWork = TRUE), basename(path))
  if (dir.exists(path)) rlang::abort("Artifact path is a directory.")
  if (.warm_start_path_exists(path) && !overwrite) {
    rlang::abort("Artifact already exists; use overwrite = TRUE to replace it.")
  }
  stage <- tempfile(pattern = ".warm-start-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(stage), add = TRUE)
  tryCatch({
    saveRDS(model, stage, compress = "xz", version = 3)
    restored <- .warm_start_read_model(stage)
    if (!identical(model, restored)) rlang::abort("Staged artifact failed lossless round-trip validation.")
    # A hard link is an atomic no-clobber publication; rename handles explicit replacement.
    ok <- if (overwrite) .warm_start_rename(stage, path) else .warm_start_link(stage, path)
    if (!isTRUE(ok)) rlang::abort("Cannot publish artifact; existing destination was not replaced.")
  }, error = function(e) rlang::abort(paste0("Cannot save warm-start model to '", path, "'."), parent = e))
  invisible(path)
}

.warm_start_rename <- function(from, to) suppressWarnings(file.rename(from, to))
.warm_start_link <- function(from, to) suppressWarnings(file.link(from, to))

.warm_start_file_path <- function(path) {
  if (!.warm_start_string(path)) rlang::abort("`path` must be one nonblank character string.")
  path.expand(path)
}

.warm_start_path_exists <- function(path) {
  link <- Sys.readlink(path)
  file.exists(path) || (!is.na(link) && nzchar(link))
}

.warm_start_read_model <- function(path) {
  tryCatch({
    model <- readRDS(path)
    .validate_warm_start_artifact(model)
    model
  }, error = function(e) rlang::abort(paste0("Cannot load warm-start artifact '", path, "'."), parent = e))
}

#' @rdname save_warm_start_model
#' @export
load_warm_start_model <- function(path = NULL, name = NULL, source = c("auto", "user", "bundled")) {
  source <- match.arg(source)
  if (is.null(path) == is.null(name)) rlang::abort("Supply exactly one of `path` or `name`.")
  if (!is.null(path)) {
    if (source != "auto") rlang::abort("`source` applies only to model names, not explicit paths.")
    path <- .warm_start_file_path(path)
  } else {
    path <- .warm_start_resolve_name(name, source)
  }
  if (!file.exists(path) || dir.exists(path)) rlang::abort(paste0("Model file does not exist: ", path))
  .warm_start_read_model(path)
}
