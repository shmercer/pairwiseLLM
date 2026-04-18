# -------------------------------------------------------------------------
# Adaptive persistence helpers.
# -------------------------------------------------------------------------

.adaptive_session_paths <- function(session_dir) {
  list(
    state = file.path(session_dir, "state.rds"),
    step_log = file.path(session_dir, "step_log.rds"),
    round_log = file.path(session_dir, "round_log.rds"),
    link_stage_log = file.path(session_dir, "link_stage_log.rds"),
    metadata = file.path(session_dir, "metadata.rds"),
    btl_fit = file.path(session_dir, "btl_fit.rds"),
    item_log_dir = file.path(session_dir, "item_log"),
    phase_a_artifact_dir = file.path(session_dir, "phase_a_artifacts")
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

.adaptive_align_log_schema_for_resume <- function(log_tbl, schema, name, fill_missing = TRUE) {
  if (!is.data.frame(log_tbl)) {
    rlang::abort(paste0("`", name, "` must be a data frame."))
  }
  out <- tibble::as_tibble(log_tbl)
  normalize_policy_col <- function(x) {
    vapply(
      as.character(x),
      function(value) {
        if (is.na(value) || value == "") {
          return(NA_character_)
        }
        .adaptive_normalize_link_transform_policy(value)
      },
      character(1),
      USE.NAMES = FALSE
    )
  }
  normalize_state_col <- function(x) {
    vapply(
      as.character(x),
      function(value) {
        if (is.na(value) || value == "") {
          return(NA_character_)
        }
        .adaptive_normalize_link_transform_state(value)
      },
      character(1),
      USE.NAMES = FALSE
    )
  }
  if (identical(name, "step_log")) {
    if ("posterior_win_prob_pre" %in% names(out) && !"posterior_win_prob_ij_pre" %in% names(out)) {
      out$posterior_win_prob_ij_pre <- out$posterior_win_prob_pre
    }
    run_mode_chr <- if ("run_mode" %in% names(out)) {
      as.character(out$run_mode)
    } else {
      rep(NA_character_, nrow(out))
    }
    run_mode_known <- !is.na(run_mode_chr) & nzchar(run_mode_chr)
    if (!"is_holdout_probe_step" %in% names(out)) {
      out$is_holdout_probe_step <- as.logical(run_mode_chr == "link_probe_holdout")
    } else {
      out$is_holdout_probe_step <- as.logical(out$is_holdout_probe_step %||% FALSE)
      out$is_holdout_probe_step[run_mode_known] <- run_mode_chr[run_mode_known] == "link_probe_holdout"
    }
    if (!"is_drift_probe_step" %in% names(out)) {
      out$is_drift_probe_step <- as.logical(run_mode_chr == "link_probe")
    } else {
      out$is_drift_probe_step <- as.logical(out$is_drift_probe_step %||% FALSE)
      out$is_drift_probe_step[run_mode_known] <- run_mode_chr[run_mode_known] == "link_probe"
    }
    if ("is_probe_step" %in% names(out)) {
      out$is_probe_step <- as.logical(out$is_probe_step %||% FALSE)
      out$is_probe_step[run_mode_known] <- run_mode_chr[run_mode_known] %in%
        c("link_probe_holdout", "link_probe")
    }
    if ("link_transform_mode" %in% names(out)) {
      if (!"link_transform_policy" %in% names(out)) {
        out$link_transform_policy <- normalize_policy_col(out$link_transform_mode)
      }
      if (!"link_transform_state" %in% names(out)) {
        out$link_transform_state <- normalize_state_col(out$link_transform_mode)
      }
      out$link_transform_mode <- NULL
    }
    if (!"link_estimation_mode" %in% names(out)) {
      is_linking_row <- rep_len(FALSE, nrow(out))
      if ("is_cross_set" %in% names(out)) {
        is_linking_row <- as.logical(out$is_cross_set %||% FALSE)
      }
      if ("run_mode" %in% names(out)) {
        run_mode_chr <- as.character(out$run_mode)
        is_linking_row <- is_linking_row |
          run_mode_chr %in% c("link_one_spoke", "link_multi_spoke", "link_probe_holdout", "link_probe")
      }
      out$link_estimation_mode <- ifelse(is_linking_row, "transform", NA_character_)
    }
  }
  if (identical(name, "link_stage_log")) {
    if ("transform_frozen" %in% names(out) && !"link_state_frozen" %in% names(out)) {
      out$link_state_frozen <- as.logical(out$transform_frozen)
    }
    if ("transform_frozen_refit_id" %in% names(out) &&
      !"link_state_frozen_refit_id" %in% names(out)) {
      out$link_state_frozen_refit_id <- as.integer(out$transform_frozen_refit_id)
    }
    out$transform_frozen <- NULL
    out$transform_frozen_refit_id <- NULL
    if ("link_transform_mode" %in% names(out)) {
      if (!"link_transform_policy" %in% names(out)) {
        out$link_transform_policy <- normalize_policy_col(out$link_transform_mode)
      }
      if (!"link_transform_state" %in% names(out)) {
        out$link_transform_state <- normalize_state_col(out$link_transform_mode)
      }
      out$link_transform_mode <- NULL
    }
    if ("ppc_calibration_id" %in% names(out)) {
      out$ppc_calibration_id <- NULL
    }
    if ("cross_set_ppc_brier_max_used" %in% names(out)) {
      out$cross_set_ppc_brier_max_used <- NULL
    }
    if ("reliability_EAP_link" %in% names(out) && !"reliability_link_global" %in% names(out)) {
      out$reliability_link_global <- suppressWarnings(as.double(out$reliability_EAP_link))
    }
    if ("stop_consecutive_pass_count" %in% names(out)) {
      if (!"stop_recent_pass_count" %in% names(out)) {
        out$stop_recent_pass_count <- as.integer(out$stop_consecutive_pass_count)
      }
      if (!"stop_recent_window_size" %in% names(out)) {
        out$stop_recent_window_size <- as.integer(out$stop_consecutive_pass_count)
      }
      out$stop_consecutive_pass_count <- NULL
    }
    if ("escalation_consecutive_pass_count" %in% names(out)) {
      if (!"escalation_recent_pass_count" %in% names(out)) {
        out$escalation_recent_pass_count <- as.integer(out$escalation_consecutive_pass_count)
      }
      if (!"escalation_recent_window_size" %in% names(out)) {
        out$escalation_recent_window_size <- as.integer(out$escalation_consecutive_pass_count)
      }
      out$escalation_consecutive_pass_count <- NULL
    }
    if ("link_transform_escalation_refits_required_used" %in% names(out)) {
      if (!"link_transform_escalation_window_refits_used" %in% names(out)) {
        out$link_transform_escalation_window_refits_used <-
          as.integer(out$link_transform_escalation_refits_required_used)
      }
      if (!"link_transform_escalation_passes_required_used" %in% names(out)) {
        out$link_transform_escalation_passes_required_used <-
          as.integer(out$link_transform_escalation_refits_required_used)
      }
      out$link_transform_escalation_refits_required_used <- NULL
    }
    if ("reliability_EAP_link" %in% names(out)) {
      out$reliability_EAP_link <- NULL
    }
    if (!"link_estimation_mode" %in% names(out)) {
      out$link_estimation_mode <- rep("transform", nrow(out))
    }
    if (!"phase_b_global_metric_uncertainty_approximation" %in% names(out)) {
      mode_vals <- as.character(out$link_estimation_mode %||% rep(NA_character_, nrow(out)))
      uncertainty_vals <- if ("link_uncertainty_approximation" %in% names(out)) {
        as.character(out$link_uncertainty_approximation)
      } else {
        rep(NA_character_, nrow(out))
      }
      fit_method_vals <- if ("link_fit_method" %in% names(out)) {
        as.character(out$link_fit_method)
      } else {
        rep(NA_character_, nrow(out))
      }
      out$phase_b_global_metric_uncertainty_approximation <- vapply(
        seq_len(nrow(out)),
        function(idx) {
          .adaptive_phase_b_global_metric_uncertainty_approximation(
            link_estimation_mode = mode_vals[[idx]],
            link_uncertainty_approximation = uncertainty_vals[[idx]],
            link_fit_method = fit_method_vals[[idx]]
          )
        },
        character(1)
      )
    }
  }
  if (identical(name, "step_log") || identical(name, "link_stage_log")) {
    out <- .adaptive_log_normalize_mode_fields(
      out,
      if (identical(name, "step_log")) schema_step_log else schema_link_stage_log,
      name
    )
  }
  if (!isTRUE(fill_missing)) {
    return(out)
  }
  schema_names <- names(schema)
  for (col in schema_names) {
    if (!col %in% names(out)) {
      out[[col]] <- rep(.adaptive_schema_typed_na(schema[[col]]), nrow(out))
    }
  }
  out <- out[, schema_names, drop = FALSE]

  for (col in schema_names) {
    type <- schema[[col]]
    value <- out[[col]]
    if (identical(type, "POSIXct")) {
      out[[col]] <- as.POSIXct(value, tz = "UTC")
    } else if (identical(type, "integer")) {
      out[[col]] <- as.integer(value)
    } else if (identical(type, "double")) {
      out[[col]] <- as.double(value)
    } else if (identical(type, "logical")) {
      out[[col]] <- as.logical(value)
    } else if (identical(type, "character")) {
      out[[col]] <- as.character(value)
    }
  }
  out
}

.adaptive_align_round_log_post_stop_columns <- function(round_log) {
  out <- tibble::as_tibble(round_log)
  n <- nrow(out)
  defaults <- c(
    max_pairs_after_stop = 0L,
    pairs_committed_after_stop = 0L
  )
  for (col in names(defaults)) {
    if (!col %in% names(out)) {
      out[[col]] <- rep.int(as.integer(defaults[[col]]), n)
    }
  }
  out
}

.adaptive_align_round_log_probe_audit_columns <- function(round_log) {
  out <- tibble::as_tibble(round_log)
  n <- nrow(out)
  defaults <- list(
    new_active_pairs_since_last_refit = rep(NA_integer_, n),
    new_probe_pairs_since_last_refit = rep(NA_integer_, n),
    new_total_cross_pairs_since_last_refit = rep(NA_integer_, n)
  )
  for (col in names(defaults)) {
    if (!col %in% names(out)) {
      out[[col]] <- defaults[[col]]
    }
  }
  ordered_existing <- intersect(names(schema_round_log), names(out))
  trailing_extra <- setdiff(names(out), ordered_existing)
  out[, c(ordered_existing, trailing_extra), drop = FALSE]
}

.adaptive_resume_backfill_legacy_linking_defaults <- function(state) {
  out <- state
  controller <- out$controller %||% list()
  if (!is.list(controller)) {
    controller <- list()
  }

  read_latest_non_missing <- function(tbl, col) {
    if (!is.data.frame(tbl) || !col %in% names(tbl) || nrow(tbl) < 1L) {
      return(NA_character_)
    }
    vals <- as.character(tbl[[col]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) < 1L) {
      return(NA_character_)
    }
    vals[[length(vals)]]
  }

  mode_missing <- is.null(controller$link_estimation_mode) ||
    length(controller$link_estimation_mode) < 1L ||
    is.na(as.character(controller$link_estimation_mode[[1L]])) ||
    !nzchar(as.character(controller$link_estimation_mode[[1L]]))
  if (isTRUE(mode_missing)) {
    log_mode <- read_latest_non_missing(out$link_stage_log, "link_estimation_mode")
    if (is.na(log_mode)) {
      log_mode <- read_latest_non_missing(out$step_log, "link_estimation_mode")
    }
    controller$link_estimation_mode <- if (is.na(log_mode)) "transform" else log_mode
  }

  lock_missing <- is.null(controller$hub_lock_mode) ||
    length(controller$hub_lock_mode) < 1L ||
    is.na(as.character(controller$hub_lock_mode[[1L]])) ||
    !nzchar(as.character(controller$hub_lock_mode[[1L]]))
  if (isTRUE(lock_missing)) {
    mode <- as.character(controller$link_estimation_mode[[1L]] %||% NA_character_)
    log_lock <- read_latest_non_missing(out$link_stage_log, "hub_lock_mode")
    controller$hub_lock_mode <- if (identical(mode, "anchored_joint")) {
      "hard_lock"
    } else if (!is.na(log_lock)) {
      log_lock
    } else {
      "soft_lock"
    }
  }

  out$controller <- controller
  out
}

.adaptive_item_log_current_schema <- function() {
  cols <- .adaptive_item_log_columns()
  int_cols <- c(
    "refit_id", "set_id", "phase_scope_set_id", "rank_raw", "rank_link",
    "degree", "pos_count_A", "pos_count_B"
  )
  lgl_cols <- c("in_phase_scope", "is_hub_item", "is_spoke_item")
  chr_cols <- c("item_id", "phase_scope")
  types <- vapply(
    cols,
    function(col) {
      if (col %in% int_cols) {
        return("integer")
      }
      if (col %in% chr_cols) {
        return("character")
      }
      if (col %in% lgl_cols) {
        return("logical")
      }
      "double"
    },
    character(1)
  )
  stats::setNames(unname(types), cols)
}

.adaptive_item_log_legacy_schema <- function() {
  c(
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
}

.adaptive_validate_item_log_resume_schema <- function(item_log, name) {
  schemas <- list(
    .adaptive_item_log_current_schema(),
    .adaptive_item_log_legacy_schema()
  )
  for (schema in schemas) {
    ok <- tryCatch(
      {
        .adaptive_validate_log_schema(item_log, schema, name)
        TRUE
      },
      error = function(e) FALSE
    )
    if (isTRUE(ok)) {
      return(invisible(NULL))
    }
  }
  rlang::abort(paste0(
    "`",
    name,
    "` does not match a supported item log schema for resume."
  ))
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
    "link_stage_log",
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

#' @keywords internal
#' @noRd
.adaptive_history_state_rebuild_state <- function(state,
                                                  validate_existing = FALSE,
                                                  context = "runtime") {
  state$history_state <- .adaptive_history_state_resolve(
    state,
    ids = as.character(state$item_ids %||% character()),
    validate_existing = validate_existing,
    context = context
  )
  state
}

.adaptive_resume_history_pairs_from_step_log <- function(state, step_log) {
  step_log <- tibble::as_tibble(step_log %||% tibble::tibble())
  if (nrow(step_log) < 1L || !all(c("pair_id", "A", "B") %in% names(step_log))) {
    return(tibble::tibble(A_id = character(), B_id = character()))
  }

  holdout_flag <- .adaptive_link_is_holdout_probe_rows(step_log)
  committed <- step_log[
    !is.na(step_log$pair_id) &
      !(holdout_flag %in% TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(committed) < 1L) {
    return(tibble::tibble(A_id = character(), B_id = character()))
  }

  ids <- as.character(state$item_ids %||% character())
  out_A <- if ("A_id" %in% names(committed)) {
    as.character(committed$A_id)
  } else {
    rep(NA_character_, nrow(committed))
  }
  out_B <- if ("B_id" %in% names(committed)) {
    as.character(committed$B_id)
  } else {
    rep(NA_character_, nrow(committed))
  }
  missing_A <- is.na(out_A) | !nzchar(out_A)
  missing_B <- is.na(out_B) | !nzchar(out_B)
  out_A[missing_A] <- as.character(ids[as.integer(committed$A[missing_A])])
  out_B[missing_B] <- as.character(ids[as.integer(committed$B[missing_B])])
  tibble::tibble(
    A_id = out_A,
    B_id = out_B
  )
}

.adaptive_resume_reconcile_refit_meta <- function(state, step_log, round_log) {
  step_log <- tibble::as_tibble(step_log %||% tibble::tibble())
  round_log <- tibble::as_tibble(round_log %||% tibble::tibble())
  state$history_pairs <- .adaptive_resume_history_pairs_from_step_log(state, step_log)
  state <- .adaptive_history_state_rebuild_state(state, validate_existing = TRUE, context = "resume")

  refit_meta <- state$refit_meta %||% list()
  if (nrow(round_log) < 1L) {
    refit_meta$last_refit_M_done <- 0L
    refit_meta$last_refit_step <- 0L
    refit_meta$last_refit_round_id <- 0L
    state$refit_meta <- refit_meta
    return(state)
  }

  round_log <- round_log[
    order(as.integer(round_log$refit_id), seq_len(nrow(round_log))),
    ,
    drop = FALSE
  ]
  last_row <- round_log[nrow(round_log), , drop = FALSE]
  last_refit_step <- as.integer(last_row$step_id_at_refit[[1L]] %||% NA_integer_)
  committed_step_count_non_holdout <- if (nrow(step_log) > 0L && "pair_id" %in% names(step_log)) {
    holdout_flag <- .adaptive_link_is_holdout_probe_rows(step_log)
    as.integer(sum(
      !is.na(step_log$pair_id) &
        !(holdout_flag %in% TRUE),
      na.rm = TRUE
    ))
  } else {
    0L
  }
  if (nrow(step_log) < 1L || !"pair_id" %in% names(step_log)) {
    refit_meta$last_refit_M_done <- as.integer(refit_meta$last_refit_M_done %||% 0L)
    refit_meta$last_refit_step <- as.integer(last_refit_step %||% 0L)
    refit_meta$last_refit_round_id <- as.integer(last_row$refit_id[[1L]] %||% nrow(round_log))
    state$refit_meta <- refit_meta
    return(state)
  }
  if (committed_step_count_non_holdout < 1L) {
    refit_meta$last_refit_M_done <- 0L
    refit_meta$last_refit_step <- as.integer(last_refit_step %||% 0L)
    refit_meta$last_refit_round_id <- as.integer(last_row$refit_id[[1L]] %||% nrow(round_log))
    state$refit_meta <- refit_meta
    return(state)
  }
  max_step_id <- if (nrow(step_log) > 0L && "step_id" %in% names(step_log)) {
    as.integer(max(as.integer(step_log$step_id), na.rm = TRUE))
  } else {
    0L
  }
  if (!is.finite(last_refit_step) || last_refit_step < 0L || last_refit_step > max_step_id) {
    rlang::abort(
      paste0(
        "Adaptive resume invariant failed: canonical `round_log$step_id_at_refit` is out of range. ",
        "last_refit_step=",
        as.integer(last_refit_step),
        ", max_step_id=",
        as.integer(max_step_id),
        "."
      )
    )
  }

  committed_at_refit <- if (nrow(step_log) > 0L && all(c("pair_id", "step_id") %in% names(step_log))) {
    holdout_flag <- .adaptive_link_is_holdout_probe_rows(step_log)
    as.integer(sum(
      !is.na(step_log$pair_id) &
        !(holdout_flag %in% TRUE) &
        as.integer(step_log$step_id) <= as.integer(last_refit_step),
      na.rm = TRUE
    ))
  } else {
    0L
  }
  committed_all_at_refit <- if (nrow(step_log) > 0L && all(c("pair_id", "step_id") %in% names(step_log))) {
    as.integer(sum(
      !is.na(step_log$pair_id) &
        as.integer(step_log$step_id) <= as.integer(last_refit_step),
      na.rm = TRUE
    ))
  } else {
    0L
  }
  logged_total_pairs <- as.integer(last_row$total_pairs_done[[1L]] %||% committed_at_refit)
  total_pairs_matches <- isTRUE(logged_total_pairs == committed_all_at_refit) ||
    isTRUE(logged_total_pairs == committed_at_refit)
  if (is.finite(logged_total_pairs) && !isTRUE(total_pairs_matches)) {
    rlang::abort(
      paste0(
        "Adaptive resume invariant failed: canonical `round_log$total_pairs_done` does not reconcile ",
        "to committed `step_log` rows at the last refit boundary. logged_total_pairs=",
        as.integer(logged_total_pairs),
        ", committed_all_at_refit=",
        as.integer(committed_all_at_refit),
        ", committed_non_holdout_at_refit=",
        as.integer(committed_at_refit),
        ", last_refit_step=",
        as.integer(last_refit_step),
        "."
      )
    )
  }

  refit_meta$last_refit_M_done <- as.integer(committed_at_refit)
  refit_meta$last_refit_step <- as.integer(last_refit_step)
  refit_meta$last_refit_round_id <- as.integer(last_row$refit_id[[1L]] %||% nrow(round_log))
  state$refit_meta <- refit_meta
  state
}

.adaptive_link_probe_resume_abort <- function(message, spoke_id = NA_integer_) {
  prefix <- "Adaptive resume probe-state invariant failed"
  if (is.finite(as.integer(spoke_id))) {
    prefix <- paste0(prefix, " for spoke_id=", as.integer(spoke_id))
  }
  rlang::abort(paste0(prefix, ": ", message, "."))
}

.adaptive_is_resumed_session <- function(state) {
  isTRUE((state$meta %||% list())$resumed_from_session) ||
    isTRUE((state$config %||% list())$resumed_from_session)
}

.adaptive_link_probe_resume_spoke_ids <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  ids <- c(
    as.integer(phase_ctx$active_spokes %||% integer()),
    as.integer(phase_ctx$ready_spokes %||% integer()),
    suppressWarnings(as.integer(names(state$linking$probe$panels_by_spoke %||% list()))),
    as.integer((state$linking$probe$realized_edges %||% tibble::tibble())$spoke_id %||% integer()),
    as.integer((state$linking$probe$prediction_cache %||% tibble::tibble())$spoke_id %||% integer()),
    as.integer((state$link_stage_log %||% tibble::tibble())$spoke_id %||% integer()),
    suppressWarnings(as.integer(names(controller$link_epoch_id_by_spoke %||% list())))
  )
  sort(unique(ids[is.finite(ids) & !is.na(ids)]))
}

.adaptive_link_probe_resume_validate_current_window <- function(state, spoke_id, panel_epoch, panel) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  required <- c("pair_id", "step_id", "link_spoke_id", "A", "B")
  if (nrow(step_log) < 1L || !all(required %in% names(step_log))) {
    return(invisible(NULL))
  }

  holdout_flag <- .adaptive_link_is_holdout_probe_rows(step_log)
  last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
  current_window_steps <- step_log[
    !is.na(step_log$pair_id) &
      as.integer(step_log$step_id) > last_refit_step &
      as.integer(step_log$link_spoke_id) == as.integer(spoke_id) &
      holdout_flag %in% TRUE,
    ,
    drop = FALSE
  ]
  if (nrow(current_window_steps) < 1L) {
    return(invisible(NULL))
  }

  ids <- as.character(state$item_ids %||% character())
  current_window_steps$pair_key <- make_unordered_key(
    ids[as.integer(current_window_steps$A)],
    ids[as.integer(current_window_steps$B)]
  )
  if (!all(as.character(current_window_steps$pair_key) %in% as.character(panel$pair_key))) {
    .adaptive_link_probe_resume_abort(
      paste0(
        "committed holdout probe steps after the last refit are not contained in the current panel ",
        "for link_epoch_id=",
        as.integer(panel_epoch)
      ),
      spoke_id = spoke_id
    )
  }

  realized_since_last_refit_n <- .adaptive_link_probe_realized_count_since_step(
    state = state,
    spoke_id = as.integer(spoke_id),
    epoch_id = as.integer(panel_epoch),
    last_step_id = as.integer(last_refit_step),
    panel = panel
  )
  if (!identical(nrow(current_window_steps), realized_since_last_refit_n)) {
    .adaptive_link_probe_resume_abort(
      paste0(
        "committed holdout probe steps after the last refit do not reconcile to canonical ",
        "`realized_edges` for link_epoch_id=",
        as.integer(panel_epoch),
        " (steps=",
        as.integer(nrow(current_window_steps)),
        ", canonical=",
        as.integer(realized_since_last_refit_n),
        ")"
      ),
      spoke_id = spoke_id
    )
  }

  invisible(NULL)
}

.adaptive_link_probe_resume_validate_spoke <- function(state, spoke_id) {
  probe <- .adaptive_link_probe_state(state)
  panel <- tibble::as_tibble(
    probe$panels_by_spoke[[as.character(as.integer(spoke_id))]] %||% .adaptive_link_probe_empty_panel()
  )
  if (nrow(panel) < 1L) {
    return(invisible(NULL))
  }

  if (!all(as.integer(panel$spoke_id) == as.integer(spoke_id))) {
    .adaptive_link_probe_resume_abort(
      "persisted probe panel rows carry a different `spoke_id` than their `panels_by_spoke` key",
      spoke_id = spoke_id
    )
  }

  panel_epoch <- unique(as.integer(panel$link_epoch_id))
  if (length(panel_epoch) != 1L || !is.finite(panel_epoch)) {
    .adaptive_link_probe_resume_abort(
      "persisted probe panel must have exactly one non-missing `link_epoch_id`",
      spoke_id = spoke_id
    )
  }
  panel_epoch <- as.integer(panel_epoch[[1L]])

  panel_id <- unique(as.character(panel$probe_panel_id))
  panel_id <- panel_id[!is.na(panel_id) & nzchar(panel_id)]
  if (length(panel_id) != 1L) {
    .adaptive_link_probe_resume_abort(
      "persisted probe panel must have exactly one non-empty `probe_panel_id`",
      spoke_id = spoke_id
    )
  }
  panel_id <- as.character(panel_id[[1L]])
  panel_planned_edges <- .adaptive_link_probe_planned_edges(panel)
  panel_reallocation_used <- .adaptive_link_probe_panel_reallocation_used(panel)

  if (anyDuplicated(as.character(panel$pair_key))) {
    .adaptive_link_probe_resume_abort(
      "persisted probe panel contains duplicate `pair_key` values",
      spoke_id = spoke_id
    )
  }

  controller <- .adaptive_controller_resolve(state)
  controller_epoch <- as.integer(
    (controller$link_epoch_id_by_spoke %||% list())[[as.character(spoke_id)]] %||% NA_integer_
  )
  if (is.finite(controller_epoch) && !identical(controller_epoch, panel_epoch)) {
    .adaptive_link_probe_resume_abort(
      paste0(
        "`controller$link_epoch_id_by_spoke`=",
        controller_epoch,
        " does not match persisted panel epoch ",
        panel_epoch
      ),
      spoke_id = spoke_id
    )
  }

  spoke_rows <- tibble::as_tibble(state$link_stage_log %||% new_link_stage_log())
  spoke_rows <- spoke_rows[as.integer(spoke_rows$spoke_id) == as.integer(spoke_id), , drop = FALSE]
  if (nrow(spoke_rows) > 0L) {
    spoke_rows <- spoke_rows[order(as.integer(spoke_rows$refit_id), seq_len(nrow(spoke_rows))), , drop = FALSE]
    last_row <- spoke_rows[nrow(spoke_rows), , drop = FALSE]
    row_epoch <- as.integer(last_row$link_epoch_id[[1L]] %||% NA_integer_)
    row_panel_id <- as.character(last_row$probe_panel_id[[1L]] %||% NA_character_)
    if (is.finite(row_epoch) && !identical(row_epoch, panel_epoch)) {
      .adaptive_link_probe_resume_abort(
        paste0(
          "latest `link_stage_log$link_epoch_id`=",
          row_epoch,
          " does not match persisted panel epoch ",
          panel_epoch
        ),
        spoke_id = spoke_id
      )
    }
    if (!is.na(row_panel_id) && nzchar(row_panel_id) && !identical(row_panel_id, panel_id)) {
      .adaptive_link_probe_resume_abort(
        paste0(
          "latest `link_stage_log$probe_panel_id`=",
          row_panel_id,
          " does not match persisted panel id ",
          panel_id
        ),
        spoke_id = spoke_id
      )
    }
    row_planned <- as.integer(last_row$probe_edges_planned[[1L]] %||% NA_integer_)
    if (is.finite(row_planned) &&
      row_planned > 0L &&
      !identical(as.integer(row_planned), as.integer(panel_planned_edges))) {
      .adaptive_link_probe_resume_abort(
        paste0(
          "latest `link_stage_log$probe_edges_planned`=",
          as.integer(row_planned),
          " does not match the canonical planned probe count ",
          as.integer(panel_planned_edges)
        ),
        spoke_id = spoke_id
      )
    }
    row_reallocation <- as.logical(last_row$probe_panel_reallocation_used[[1L]] %||% NA)
    if (!is.na(row_reallocation) &&
      !identical(isTRUE(row_reallocation), isTRUE(panel_reallocation_used))) {
      .adaptive_link_probe_resume_abort(
        paste0(
          "latest `link_stage_log$probe_panel_reallocation_used`=",
          isTRUE(row_reallocation),
          " does not match the canonical panel construction value ",
          isTRUE(panel_reallocation_used)
        ),
        spoke_id = spoke_id
      )
    }
  }

  realized_edges <- .adaptive_link_probe_realized_log_for_epoch(
    state = state,
    spoke_id = as.integer(spoke_id),
    epoch_id = as.integer(panel_epoch)
  )
  if (nrow(realized_edges) > 0L) {
    bad_key <- !as.character(realized_edges$pair_key) %in% as.character(panel$pair_key)
    if (any(bad_key)) {
      .adaptive_link_probe_resume_abort(
        "persisted `realized_edges` include pair keys not present in the current panel",
        spoke_id = spoke_id
      )
    }
    bad_panel_id <- !is.na(realized_edges$probe_panel_id) &
      nzchar(realized_edges$probe_panel_id) &
      as.character(realized_edges$probe_panel_id) != panel_id
    if (any(bad_panel_id)) {
      .adaptive_link_probe_resume_abort(
        "persisted `realized_edges$probe_panel_id` does not match the current panel id",
        spoke_id = spoke_id
      )
    }
  }

  realized_count <- .adaptive_link_probe_realized_count(
    state = state,
    spoke_id = as.integer(spoke_id),
    epoch_id = as.integer(panel_epoch)
  )
  if (nrow(spoke_rows) > 0L) {
    last_row <- spoke_rows[nrow(spoke_rows), , drop = FALSE]
    row_realized <- as.integer(last_row$probe_edges_realized[[1L]] %||% NA_integer_)
    last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
    current_window_realized <- .adaptive_link_probe_realized_count_since_step(
      state = state,
      spoke_id = as.integer(spoke_id),
      epoch_id = as.integer(panel_epoch),
      last_step_id = as.integer(last_refit_step),
      panel = panel
    )
    delta_from_row <- as.integer(as.integer(realized_count) - row_realized)
    if (is.finite(row_realized) &&
      (delta_from_row < 0L || delta_from_row > current_window_realized)) {
      .adaptive_link_probe_resume_abort(
        paste0(
          "latest `link_stage_log$probe_edges_realized`=",
          row_realized,
          " is inconsistent with canonical realized count ",
          realized_count,
          " given current-window realized probes ",
          current_window_realized,
          " after the last refit"
        ),
        spoke_id = spoke_id
      )
    }
  }
  .adaptive_link_probe_resume_validate_current_window(
    state = state,
    spoke_id = as.integer(spoke_id),
    panel_epoch = as.integer(panel_epoch),
    panel = panel
  )

  invisible(NULL)
}

.adaptive_validate_probe_state_for_resume <- function(state) {
  probe <- .adaptive_link_probe_state(state)
  probe <- .adaptive_link_probe_realized_index_reconcile(
    probe,
    context = "resume",
    validate_existing = TRUE
  )
  state$linking <- state$linking %||% list()
  state$linking$probe <- probe

  spoke_ids <- .adaptive_link_probe_resume_spoke_ids(state)
  if (length(spoke_ids) < 1L) {
    return(state)
  }

  for (spoke_id in spoke_ids) {
    .adaptive_link_probe_resume_validate_spoke(state, spoke_id = spoke_id)
  }
  state
}

.adaptive_read_session_metadata <- function(paths) {
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
  metadata
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

.adaptive_write_item_log_files <- function(item_log_list,
                                          item_log_dir,
                                          overwrite_existing = TRUE,
                                          trim_stale = FALSE) {
  if (!is.list(item_log_list) || length(item_log_list) == 0L) {
    if (isTRUE(trim_stale) && dir.exists(item_log_dir)) {
      unlink(item_log_dir, recursive = TRUE, force = TRUE)
    }
    return(invisible(NULL))
  }
  dir.create(item_log_dir, recursive = TRUE, showWarnings = FALSE)
  refit_ids <- seq_along(item_log_list)
  paths <- .adaptive_item_log_paths(item_log_dir, refit_ids)
  if (isTRUE(trim_stale) && dir.exists(item_log_dir)) {
    existing_paths <- list.files(
      item_log_dir,
      pattern = "^refit_\\d+\\.rds$",
      full.names = TRUE
    )
    stale_paths <- setdiff(existing_paths, unname(paths))
    if (length(stale_paths) > 0L) {
      unlink(stale_paths, force = TRUE)
    }
  }
  for (idx in seq_along(item_log_list)) {
    if (!isTRUE(overwrite_existing) && file.exists(paths[[idx]])) {
      next
    }
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
#' canonical adaptive session metadata shape. Validation is strict:
#' added/removed/reordered columns in persisted logs are treated as schema
#' incompatibilities and abort resume.
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

  metadata <- .adaptive_read_session_metadata(paths)

  step_log <- .adaptive_align_log_schema_for_resume(
    read_log(paths$step_log),
    schema_step_log,
    "step_log",
    fill_missing = FALSE
  )
  round_log <- read_log(paths$round_log)
  round_log <- .adaptive_align_round_log_post_stop_columns(round_log)
  round_log <- .adaptive_align_round_log_probe_audit_columns(round_log)
  link_stage_log <- if (file.exists(paths$link_stage_log)) {
    read_log(paths$link_stage_log)
  } else {
    new_link_stage_log()
  }
  link_stage_log <- .adaptive_align_log_schema_for_resume(link_stage_log, schema_link_stage_log, "link_stage_log")
  .adaptive_validate_log_schema(step_log, schema_step_log, "step_log")
  .adaptive_validate_log_schema(round_log, schema_round_log, "round_log")
  .adaptive_validate_log_schema(link_stage_log, schema_link_stage_log, "link_stage_log")
  item_log_list <- .adaptive_read_item_log_files(paths$item_log_dir)
  if (length(item_log_list) > 0L) {
    for (idx in seq_along(item_log_list)) {
      .adaptive_validate_item_log_resume_schema(
        item_log_list[[idx]],
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
#' are atomic at file level to reduce partial-write risk. Persisted
#' \code{step_log}/\code{round_log} files keep the full canonical schemas, so
#' resume preserves expanded audit fields without recomputation.
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
  phase_a_artifacts <- state$linking$phase_a$artifacts %||% list()

  if (!isTRUE(overwrite)) {
    .adaptive_abort_if_exists(c(
      paths$state,
      paths$step_log,
      paths$round_log,
      paths$link_stage_log,
      paths$metadata,
      paths$btl_fit,
      paths$item_log_dir,
      paths$phase_a_artifact_dir
    ))
  } else {
    if (is.null(state$btl_fit) && file.exists(paths$btl_fit)) {
      file.remove(paths$btl_fit)
    }
    if (!isTRUE(state$config$persist_item_log) && dir.exists(paths$item_log_dir)) {
      unlink(paths$item_log_dir, recursive = TRUE, force = TRUE)
    }
    if ((!is.list(phase_a_artifacts) || length(phase_a_artifacts) < 1L) &&
      dir.exists(paths$phase_a_artifact_dir)) {
      unlink(paths$phase_a_artifact_dir, recursive = TRUE, force = TRUE)
    }
  }

  state <- .adaptive_link_probe_realized_index_rebuild_state(
    state,
    context = "save",
    validate_existing = FALSE
  )
  state <- .adaptive_history_state_rebuild_state(state, validate_existing = TRUE, context = "save")

  metadata <- list(
    schema_version = as.character(state$meta$schema_version %||% "adaptive-session"),
    package_version = as.character(utils::packageVersion("pairwiseLLM")),
    n_items = as.integer(state$n_items)
  )

  write_log(tibble::as_tibble(state$step_log), paths$step_log)
  write_log(tibble::as_tibble(state$round_log), paths$round_log)
  write_log(tibble::as_tibble(state$link_stage_log %||% new_link_stage_log()), paths$link_stage_log)
  .adaptive_write_atomic(metadata, paths$metadata)
  .adaptive_write_atomic(state, paths$state)

  if (!is.null(state$btl_fit)) {
    .adaptive_write_atomic(state$btl_fit, paths$btl_fit)
  }

  if (isTRUE(state$config$persist_item_log)) {
    .adaptive_write_item_log_files(
      state$item_log,
      paths$item_log_dir,
      overwrite_existing = !isTRUE(overwrite),
      trim_stale = isTRUE(overwrite)
    )
  }
  .adaptive_write_phase_a_artifacts(
    phase_a_artifacts,
    paths$phase_a_artifact_dir,
    overwrite_existing = !isTRUE(overwrite),
    trim_stale = isTRUE(overwrite)
  )

  invisible(session_dir)
}

#' Load an adaptive session from disk.
#'
#' @details
#' Restores a persisted Adaptive state and revalidates basic invariants such
#' as schema version, required state fields, and index ranges in
#' \code{step_log}. If per-refit item logs are found on disk, they are loaded
#' into \code{state$item_log} and persistence is marked as enabled. Resume uses
#' strict schema validation for canonical logs; incompatible saved schemas abort
#' with explicit errors.
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
  paths <- .adaptive_session_paths(session_dir)
  required <- c(paths$state, paths$step_log, paths$round_log, paths$metadata)
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) {
    rlang::abort("Session directory is missing required artifacts.")
  }
  metadata <- .adaptive_read_session_metadata(paths)

  state <- readRDS(paths$state)
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state.rds` does not contain an adaptive_state object.")
  }

  state <- .adaptive_validate_state_for_resume(state)
  state$meta$schema_version <- metadata$schema_version
  state$linking <- state$linking %||% list()
  state$linking$probe <- .adaptive_link_probe_state(state)

  step_log <- .adaptive_align_log_schema_for_resume(
    read_log(paths$step_log),
    schema_step_log,
    "step_log",
    fill_missing = TRUE
  )
  round_log <- read_log(paths$round_log)
  round_log <- .adaptive_align_round_log_post_stop_columns(round_log)
  round_log <- .adaptive_align_round_log_probe_audit_columns(round_log)
  link_stage_log <- if (file.exists(paths$link_stage_log)) {
    read_log(paths$link_stage_log)
  } else {
    new_link_stage_log()
  }

  .adaptive_validate_log_schema(step_log, schema_step_log, "step_log")
  .adaptive_validate_log_schema(round_log, schema_round_log, "round_log")
  link_stage_log <- .adaptive_align_log_schema_for_resume(link_stage_log, schema_link_stage_log, "link_stage_log")
  .adaptive_validate_log_schema(link_stage_log, schema_link_stage_log, "link_stage_log")

  state$step_log <- tibble::as_tibble(step_log)
  state$round_log <- tibble::as_tibble(round_log)
  state$link_stage_log <- tibble::as_tibble(link_stage_log)
  state <- .adaptive_resume_backfill_legacy_linking_defaults(state)

  if (file.exists(paths$btl_fit)) {
    state$btl_fit <- readRDS(paths$btl_fit)
  }

  item_log_list <- .adaptive_read_item_log_files(paths$item_log_dir)
  if (length(item_log_list) > 0L) {
    state$item_log <- item_log_list
    state$config$persist_item_log <- TRUE
  }
  phase_a_artifacts <- .adaptive_read_phase_a_artifacts(paths$phase_a_artifact_dir)
  if (length(phase_a_artifacts) > 0L) {
    state$linking <- state$linking %||% list()
    state$linking$phase_a <- state$linking$phase_a %||% list(
      set_status = .adaptive_phase_a_empty_state(unique(as.integer(state$items$set_id))),
      artifacts = list(),
      ready_for_phase_b = FALSE,
      strict_ready_for_phase_b = FALSE,
      required_sets = as.integer(sort(unique(as.integer(state$items$set_id)))),
      set_stop_pass_by_set = list(),
      phase = "phase_a",
      phase_b_started_at_step = NA_integer_
    )
    state$linking$phase_a$artifacts <- phase_a_artifacts
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

  state <- .adaptive_resume_reconcile_refit_meta(
    state = state,
    step_log = state$step_log,
    round_log = state$round_log
  )
  state <- .adaptive_history_state_rebuild_state(state, validate_existing = TRUE, context = "load")
  state <- .adaptive_link_refit_summary_rebuild_current(state)
  state <- .adaptive_link_probe_realized_index_rebuild_state(
    state,
    context = "load",
    validate_existing = TRUE
  )
  state$controller <- .adaptive_controller_resolve(state)

  state$config$session_dir <- session_dir
  state$config$resumed_from_session <- TRUE
  state$meta$resumed_from_session <- TRUE
  state <- .adaptive_phase_a_prepare(state)
  state <- .adaptive_validate_probe_state_for_resume(state)
  state
}
