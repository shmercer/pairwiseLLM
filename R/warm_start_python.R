#' Check the optional warm-start feature environment
#'
#' @inheritParams extract_warm_start_features
#' @return A list with `available`, `python`, `expected`, `observed`, and `problems`.
#'   Availability requires the pinned versions, resource contents, and pipeline
#'   capabilities. Missing prerequisites are reported without throwing an error;
#'   malformed arguments or missing installed metadata are errors.
#' @details
#' This explicitly invoked check may initialize reticulate's Python interpreter
#' and load the spaCy model to verify its capabilities. It never installs software
#' or downloads resources. Once Python is initialized, selecting a different
#' interpreter requires restarting R. Use `reticulate::use_virtualenv()` or
#' `reticulate::use_condaenv()` with `required = TRUE` before this call, or supply
#' `python` directly. See the installed `python/README.md` for explicit setup.
#' @examples
#' \dontrun{
#' warm_start_python_status(python = "/path/to/venv/bin/python")
#' }
#' @export
warm_start_python_status <- function(schema = "writing_features_v1", python = NULL) {
  warm_start_feature_schema(schema)
  .warm_start_python_argument(python)
  path <- .warm_start_python_artifact("audit-environment.json")
  if (!nzchar(path) || !file.exists(path)) {
    rlang::abort("Installed Python environment metadata is missing or corrupt. Reinstall pairwiseLLM.")
  }
  expected <- tryCatch(jsonlite::read_json(path), error = function(e) {
    rlang::abort("Installed Python environment metadata is missing or corrupt. Reinstall pairwiseLLM.")
  })
  result <- tryCatch(
    .warm_start_python_request(list(operation = "status", schema = schema), python),
    error = function(e) list(problem = conditionMessage(e))
  )
  list(
    available = is.null(result$problem),
    python = if (is.null(result$python)) python else result$python,
    expected = list(python = expected$python, packages = expected$packages),
    observed = result$observed,
    problems = if (is.null(result$problem)) character() else result$problem
  )
}

.warm_start_python_argument <- function(python) {
  if (!is.null(python) && (!is.character(python) || length(python) != 1L ||
      is.na(python) || !nzchar(python))) {
    rlang::abort("`python` must be NULL or one nonempty interpreter path.")
  }
}

.warm_start_python_path <- function(path) {
  # Preserve venv identity: resolving the interpreter symlink alone loses it.
  file.path(normalizePath(dirname(path), winslash = "/", mustWork = FALSE), basename(path))
}

.warm_start_python_module <- function(python) {
  .warm_start_python_argument(python)
  if (!.warm_start_has_reticulate()) {
    rlang::abort("Install the optional R package 'reticulate' to extract writing features.")
  }
  configured <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (identical(configured, "managed")) {
    rlang::abort("Select an existing Python interpreter; RETICULATE_PYTHON='managed' is not supported.")
  }
  if (!is.null(python) && nzchar(configured) &&
      .warm_start_python_path(python) != .warm_start_python_path(configured)) {
    rlang::abort("`python` conflicts with RETICULATE_PYTHON. Correct the selection before extraction.")
  }
  if (reticulate::py_available(initialize = FALSE)) {
    active <- reticulate::py_config()$python
    requested <- if (is.null(python)) configured else python
    if (nzchar(requested) && .warm_start_python_path(active) != .warm_start_python_path(requested)) {
      rlang::abort("A different Python interpreter is already initialized. Restart R and select the required Python.")
    }
  } else {
    if (is.null(python)) {
      config <- reticulate::py_discover_config()
      python <- config$python
    }
    if (is.null(python) || !file.exists(python) || dir.exists(python)) {
      rlang::abort("No existing Python interpreter found. Provision the documented environment and supply `python`.")
    }
    reticulate::use_python(python, required = TRUE)
  }
  path <- .warm_start_python_artifact()
  if (!file.exists(file.path(path, "pairwisellm_warm_start.py"))) {
    rlang::abort("Installed Python extractor is missing. Reinstall pairwiseLLM.")
  }
  reticulate::import_from_path("pairwisellm_warm_start", path = path, convert = TRUE)
}

.warm_start_python_request <- function(request, python) {
  # Scope these settings to explicit calls; never change the user's startup setup.
  settings <- c(RETICULATE_USE_MANAGED_VENV = "no", RETICULATE_AUTOCONFIGURE = "FALSE")
  old <- stats::setNames(Sys.getenv(names(settings), unset = NA_character_), names(settings))
  on.exit({
    Sys.unsetenv(names(old)[is.na(old)])
    if (any(!is.na(old))) do.call(Sys.setenv, as.list(old[!is.na(old)]))
  }, add = TRUE)
  do.call(Sys.setenv, as.list(settings))
  module <- tryCatch(.warm_start_python_module(python), error = function(e) {
    rlang::abort(paste("Warm-start Python setup failed:", conditionMessage(e)))
  })
  payload <- jsonlite::toJSON(request, auto_unbox = TRUE, null = "null", digits = NA)
  response <- tryCatch(module$request_json(payload), error = function(e) {
    rlang::abort("Python extraction failed. Run warm_start_python_status() and check the pinned environment.")
  })
  response <- tryCatch(jsonlite::fromJSON(response, simplifyVector = FALSE), error = function(e) {
    rlang::abort("Malformed Python response: expected a JSON result.")
  })
  if (!is.list(response) || !is.logical(response$ok) || length(response$ok) != 1L || is.na(response$ok)) {
    rlang::abort("Malformed Python response: missing success status.")
  }
  if (!response$ok) {
    problem <- response$error
    if (!is.character(problem) || length(problem) != 1L || is.na(problem)) {
      rlang::abort("Malformed Python response: missing error message.")
    }
    rlang::abort(paste("Warm-start feature extraction:", problem))
  }
  if (!is.list(response$result)) rlang::abort("Malformed Python response: missing result.")
  for (message in response$warnings) {
    if (is.character(message) && length(message) == 1L) rlang::warn(message)
  }
  response$result
}

.warm_start_has_reticulate <- function() {
  requireNamespace("reticulate", quietly = TRUE)
}

.warm_start_python_artifact <- function(...) {
  system.file("python", ..., package = "pairwiseLLM")
}
