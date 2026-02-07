# -------------------------------------------------------------------------
# Adaptive persistence helpers.
# -------------------------------------------------------------------------

.adaptive_session_paths <- function(session_dir) {
  list(
    state = file.path(session_dir, "state.rds"),
    step_log = file.path(session_dir, "step_log.rds"),
    round_log = file.path(session_dir, "round_log.rds"),
    metadata = file.path(session_dir, "metadata.rds"),
    btl_fit = file.path(session_dir, "btl_fit.rds"),
    item_log_dir = file.path(session_dir, "item_log")
  )
}

.adaptive_abort_if_exists <- function(paths) {
  exists <- vapply(paths, file.exists, logical(1))
  if (any(exists)) {
    rlang::abort("Session directory already contains saved artifacts.")
  }
}

.adaptive_write_atomic <- function(obj, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  tmp <- tempfile("adaptive_tmp_", tmpdir = dir)
  saveRDS(obj, tmp)
  if (file.exists(path)) {
    file.remove(path)
  }
  if (!file.rename(tmp, path)) {
    if (file.exists(tmp)) {
      file.remove(tmp)
    }
    rlang::abort(paste0("Failed to write file: ", path))
  }
}

write_log <- function(x, path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    rlang::abort("`path` must be a single, non-missing string.")
  }
  if (grepl("\\.parquet$", path, ignore.case = TRUE)) {
    rlang::abort("Parquet logs are not supported in this build.")
  }
  if (!grepl("\\.rds$", path, ignore.case = TRUE)) {
    rlang::abort("`path` must end with .rds.")
  }
  .adaptive_write_atomic(x, path)
}

read_log <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    rlang::abort("`path` must be a single, non-missing string.")
  }
  if (grepl("\\.parquet$", path, ignore.case = TRUE)) {
    rlang::abort("Parquet logs are not supported in this build.")
  }
  if (!file.exists(path)) {
    rlang::abort(paste0("Missing log file: ", path))
  }
  readRDS(path)
}

.adaptive_validate_log_schema <- function(log_tbl, schema, name) {
  if (!is.data.frame(log_tbl)) {
    rlang::abort(paste0("`", name, "` must be a data frame."))
  }
  log_tbl <- tibble::as_tibble(log_tbl)
  schema_names <- names(schema)
  missing <- setdiff(schema_names, names(log_tbl))
  extra <- setdiff(names(log_tbl), schema_names)
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "`",
      name,
      "` is missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }
  if (length(extra) > 0L) {
    rlang::abort(paste0(
      "`",
      name,
      "` has unexpected columns: ",
      paste(extra, collapse = ", "),
      "."
    ))
  }
  if (!identical(names(log_tbl), schema_names)) {
    rlang::abort(paste0("`", name, "` column order does not match canonical schema."))
  }

  for (col in schema_names) {
    type <- schema[[col]]
    value <- log_tbl[[col]]
    is_ok <- FALSE
    if (identical(type, "POSIXct")) {
      is_ok <- inherits(value, "POSIXct")
    } else if (identical(type, "integer")) {
      is_ok <- is.integer(value)
    } else if (identical(type, "double")) {
      is_ok <- is.double(value)
    } else if (identical(type, "logical")) {
      is_ok <- is.logical(value)
    } else if (identical(type, "character")) {
      is_ok <- is.character(value)
    }
    if (!isTRUE(is_ok)) {
      rlang::abort(paste0("`", name, "$", col, "` does not match canonical type `", type, "`."))
    }
  }
  log_tbl
}

.adaptive_validate_state_for_resume <- function(state) {
  required <- c(
    "item_ids",
    "item_index",
    "items",
    "n_items",
    "history_pairs",
    "step_log",
    "round_log",
    "item_log",
    "item_step_log",
    "trueskill_state",
    "refit_meta",
    "config",
    "meta"
  )
  missing <- setdiff(required, names(state))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "State is missing required fields: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }
  if (!is.list(state$item_log)) {
    rlang::abort("`state$item_log` must be a list.")
  }
  if (!tibble::is_tibble(state$item_step_log)) {
    rlang::abort("`state$item_step_log` must be a tibble.")
  }
  state
}

.adaptive_item_log_paths <- function(item_log_dir, refit_ids) {
  vapply(
    refit_ids,
    function(refit_id) {
      file.path(item_log_dir, sprintf("refit_%04d.rds", as.integer(refit_id)))
    },
    character(1)
  )
}

.adaptive_write_item_log_files <- function(item_log_list, item_log_dir) {
  if (!is.list(item_log_list) || length(item_log_list) == 0L) {
    return(invisible(NULL))
  }
  dir.create(item_log_dir, recursive = TRUE, showWarnings = FALSE)
  refit_ids <- seq_along(item_log_list)
  paths <- .adaptive_item_log_paths(item_log_dir, refit_ids)
  for (idx in seq_along(item_log_list)) {
    write_log(item_log_list[[idx]], paths[[idx]])
  }
  invisible(NULL)
}

.adaptive_read_item_log_files <- function(item_log_dir) {
  if (!dir.exists(item_log_dir)) {
    return(list())
  }
  paths <- list.files(item_log_dir, pattern = "^refit_\\d+\\.rds$", full.names = TRUE)
  if (length(paths) == 0L) {
    return(list())
  }
  paths <- paths[order(paths)]
  lapply(paths, read_log)
}

#' Validate an adaptive session directory.
#'
#' @details
#' Verifies that required session artifacts exist and that serialized logs match
#' canonical schemas for \code{step_log} and \code{round_log}. This check is
#' intended as a preflight for [load_adaptive_session()] and enforces the
#' canonical adaptive session metadata shape.
#'
#' @param session_dir Directory containing session artifacts.
#'
#' @return A metadata list containing at least \code{schema_version},
#'   \code{package_version}, and \code{n_items}.
#'
#' @examples
#' dir <- tempfile("pwllm-session-")
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' save_adaptive_session(state, dir, overwrite = TRUE)
#' validate_session_dir(dir)
#'
#' @seealso [save_adaptive_session()], [load_adaptive_session()]
#'
#' @family adaptive persistence
#' @export
validate_session_dir <- function(session_dir) {
  if (!is.character(session_dir) || length(session_dir) != 1L || is.na(session_dir)) {
    rlang::abort("`session_dir` must be a single, non-missing string.")
  }
  if (!dir.exists(session_dir)) {
    rlang::abort("`session_dir` does not exist.")
  }
  paths <- .adaptive_session_paths(session_dir)
  required <- c(paths$state, paths$step_log, paths$round_log, paths$metadata)
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) {
    rlang::abort("Session directory is missing required artifacts.")
  }

  metadata <- readRDS(paths$metadata)
  if (!is.list(metadata)) {
    rlang::abort("Session metadata must be a named list.")
  }
  schema_version <- metadata$schema_version %||% NA_character_
  if (!is.character(schema_version) ||
    length(schema_version) != 1L ||
    is.na(schema_version) ||
    schema_version == "") {
    rlang::abort("Session metadata `schema_version` must be a non-empty string.")
  }
  n_items <- metadata$n_items %||% NA_integer_
  if (!.adaptive_is_integerish(n_items) || length(n_items) != 1L || is.na(n_items) || n_items < 1L) {
    rlang::abort("Session metadata `n_items` must be a positive integer.")
  }

  step_log <- read_log(paths$step_log)
  round_log <- read_log(paths$round_log)
  .adaptive_validate_log_schema(step_log, schema_step_log, "step_log")
  .adaptive_validate_log_schema(round_log, schema_round_log, "round_log")
  item_log_list <- .adaptive_read_item_log_files(paths$item_log_dir)
  if (length(item_log_list) > 0L) {
    item_schema <- c(
      refit_id = "integer",
      item_id = "character",
      theta_mean = "double",
      `theta_p2.5` = "double",
      `theta_p5` = "double",
      `theta_p50` = "double",
      `theta_p95` = "double",
      `theta_p97.5` = "double",
      theta_sd = "double",
      rank_mean = "double",
      degree = "integer",
      pos_count_A = "integer",
      pos_count_B = "integer"
    )
    for (idx in seq_along(item_log_list)) {
      .adaptive_validate_log_schema(
        item_log_list[[idx]],
        item_schema,
        paste0("item_log[[", idx, "]]")
      )
    }
  }

  metadata
}

#' Save an adaptive session to disk.
#'
#' @details
#' Saves canonical Adaptive artifacts under \code{session_dir}:
#' \code{state.rds}, \code{step_log.rds}, \code{round_log.rds},
#' \code{metadata.rds}, optional \code{btl_fit.rds}, and optional per-refit item
#' log files when \code{state$config$persist_item_log} is \code{TRUE}. Writes
#' are atomic at file level to reduce partial-write risk.
#'
#' @param state Adaptive state.
#' @param session_dir Directory to write session artifacts.
#' @param overwrite Logical; overwrite existing artifacts.
#'
#' @return The \code{session_dir} path, invisibly.
#'
#' @examples
#' dir <- tempfile("pwllm-session-")
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' save_adaptive_session(state, dir, overwrite = TRUE)
#'
#' @seealso [validate_session_dir()], [load_adaptive_session()]
#'
#' @family adaptive persistence
#' @export
save_adaptive_session <- function(state, session_dir, overwrite = FALSE) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (!is.character(session_dir) || length(session_dir) != 1L || is.na(session_dir)) {
    rlang::abort("`session_dir` must be a single, non-missing string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    rlang::abort("`overwrite` must be TRUE or FALSE.")
  }

  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- .adaptive_session_paths(session_dir)

  if (!isTRUE(overwrite)) {
    .adaptive_abort_if_exists(c(
      paths$state,
      paths$step_log,
      paths$round_log,
      paths$metadata,
      paths$btl_fit,
      paths$item_log_dir
    ))
  } else {
    if (is.null(state$btl_fit) && file.exists(paths$btl_fit)) {
      file.remove(paths$btl_fit)
    }
    if (dir.exists(paths$item_log_dir)) {
      unlink(paths$item_log_dir, recursive = TRUE, force = TRUE)
    }
  }

  metadata <- list(
    schema_version = as.character(state$meta$schema_version %||% "adaptive-session"),
    package_version = as.character(utils::packageVersion("pairwiseLLM")),
    n_items = as.integer(state$n_items)
  )

  write_log(tibble::as_tibble(state$step_log), paths$step_log)
  write_log(tibble::as_tibble(state$round_log), paths$round_log)
  .adaptive_write_atomic(metadata, paths$metadata)
  .adaptive_write_atomic(state, paths$state)

  if (!is.null(state$btl_fit)) {
    .adaptive_write_atomic(state$btl_fit, paths$btl_fit)
  }

  if (isTRUE(state$config$persist_item_log)) {
    .adaptive_write_item_log_files(state$item_log, paths$item_log_dir)
  }

  invisible(session_dir)
}

#' Load an adaptive session from disk.
#'
#' @details
#' Restores a persisted Adaptive state and revalidates basic invariants such
#' as schema version, required state fields, and index ranges in
#' \code{step_log}. If per-refit item logs are found on disk, they are loaded
#' into \code{state$item_log} and persistence is marked as enabled.
#'
#' @param session_dir Directory containing session artifacts.
#'
#' @return An \code{adaptive_state} object ready for resume.
#'
#' @examples
#' dir <- tempfile("pwllm-session-")
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' save_adaptive_session(state, dir, overwrite = TRUE)
#' restored <- load_adaptive_session(dir)
#' summarize_adaptive(restored)
#'
#' @seealso [save_adaptive_session()], [validate_session_dir()], [adaptive_rank_resume()]
#'
#' @family adaptive persistence
#' @export
load_adaptive_session <- function(session_dir) {
  metadata <- validate_session_dir(session_dir)
  paths <- .adaptive_session_paths(session_dir)

  state <- readRDS(paths$state)
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state.rds` does not contain an adaptive_state object.")
  }

  state <- .adaptive_validate_state_for_resume(state)
  state$meta$schema_version <- metadata$schema_version

  step_log <- read_log(paths$step_log)
  round_log <- read_log(paths$round_log)

  state$step_log <- tibble::as_tibble(step_log)
  state$round_log <- tibble::as_tibble(round_log)

  if (file.exists(paths$btl_fit)) {
    state$btl_fit <- readRDS(paths$btl_fit)
  }

  item_log_list <- .adaptive_read_item_log_files(paths$item_log_dir)
  if (length(item_log_list) > 0L) {
    state$item_log <- item_log_list
    state$config$persist_item_log <- TRUE
  }

  ids <- as.character(state$item_ids %||% character())
  if (length(ids) == 0L) {
    rlang::abort("State is missing `item_ids`.")
  }
  if (length(ids) != as.integer(metadata$n_items)) {
    rlang::abort("Session metadata does not match state item count.")
  }

  A <- state$step_log$A
  B <- state$step_log$B
  has_a <- !is.na(A)
  has_b <- !is.na(B)
  if (any(has_a != has_b)) {
    rlang::abort("`step_log` contains incomplete item indices.")
  }
  ok_idx <- has_a & has_b
  if (any(ok_idx)) {
    invalid <- A[ok_idx] < 1L |
      A[ok_idx] > length(ids) |
      B[ok_idx] < 1L |
      B[ok_idx] > length(ids)
    if (any(invalid)) {
      rlang::abort("`step_log` contains invalid item indices.")
    }
  }

  state$config$session_dir <- session_dir
  state
}
