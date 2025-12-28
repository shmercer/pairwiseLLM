# ---- checkpointing (internal) ----

#' Internal: write a resumable checkpoint to disk
#'
#' This helper is used by the bt_run_* orchestration runners to persist enough
#' state to resume after an error/interrupt. Checkpoints are stored as
#' compressed RDS files inside `checkpoint_dir`.
#'
#' @param checkpoint_dir Directory to write checkpoint files into.
#' @param payload A named list containing runner state.
#' @param basename Base filename (without extension). Default "run_state".
#' @param round Optional integer round index for per-round snapshots.
#' @param overwrite Logical; if FALSE and the target path exists, do nothing.
#'
#' @return Invisibly, the path written to.
#' @keywords internal
.bt_write_checkpoint <- function(checkpoint_dir,
                                 payload,
                                 basename = "run_state",
                                 round = NULL,
                                 overwrite = TRUE) {
  if (is.null(checkpoint_dir) || is.na(checkpoint_dir) || !nzchar(checkpoint_dir)) {
    return(invisible(NULL))
  }

  checkpoint_dir <- as.character(checkpoint_dir)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  base <- basename
  if (!is.null(round)) {
    round <- as.integer(round)
    if (is.na(round)) round <- NULL
  }

  file_name <- if (is.null(round)) {
    paste0(base, ".rds")
  } else {
    paste0(base, "_round_", sprintf("%03d", round), ".rds")
  }

  path <- file.path(checkpoint_dir, file_name)
  if (!isTRUE(overwrite) && file.exists(path)) {
    return(invisible(path))
  }

  saveRDS(payload, path, compress = "xz")
  invisible(path)
}


#' Internal: read a resumable checkpoint from disk
#'
#' @param checkpoint_dir Directory containing `run_state.rds`.
#' @param basename Base filename (without extension). Default "run_state".
#'
#' @return The checkpoint payload list.
#' @keywords internal
.bt_read_checkpoint <- function(checkpoint_dir, basename = "run_state") {
  if (is.null(checkpoint_dir) || is.na(checkpoint_dir) || !nzchar(checkpoint_dir)) {
    stop("`resume_from` must be a non-empty directory path.", call. = FALSE)
  }
  checkpoint_dir <- as.character(checkpoint_dir)
  path <- file.path(checkpoint_dir, paste0(basename, ".rds"))
  if (!file.exists(path)) {
    stop("No checkpoint found at: ", path, call. = FALSE)
  }
  readRDS(path)
}


#' Internal: ensure a checkpoint payload is compatible with this run
#'
#' @param chk Checkpoint payload.
#' @param run_type Expected run_type.
#' @param ids Expected IDs.
#'
#' @return `chk` (invisibly) if compatible.
#' @keywords internal
.bt_validate_checkpoint <- function(chk, run_type, ids) {
  if (!is.list(chk)) stop("Invalid checkpoint payload (not a list).", call. = FALSE)
  if (is.null(chk$run_type) || !identical(as.character(chk$run_type), as.character(run_type))) {
    stop("Checkpoint run_type mismatch. Expected `", run_type, "`.", call. = FALSE)
  }
  if (!is.null(chk$ids)) {
    ids_chk <- sort(as.character(chk$ids))
    ids_now <- sort(as.character(ids))
    if (length(ids_chk) != length(ids_now) || any(ids_chk != ids_now)) {
      stop("Checkpoint sample IDs do not match current `samples$ID`.", call. = FALSE)
    }
  }
  invisible(chk)
}
