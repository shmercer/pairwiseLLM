# -------------------------------------------------------------------------
# Adaptive v3 console progress reporting
# -------------------------------------------------------------------------

.adaptive_progress_level <- function(config) {
  level <- config$progress_level %||% "refit"
  if (!is.character(level) || length(level) != 1L || is.na(level)) {
    return("refit")
  }
  level
}

.adaptive_progress_value <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0L) {
    return("NA")
  }
  value <- x[[1L]]
  if (is.na(value)) {
    return("NA")
  }
  if (is.logical(value)) {
    return(ifelse(value, "TRUE", "FALSE"))
  }
  if (is.numeric(value)) {
    if (is.finite(value) && abs(value - round(value)) < 1e-8) {
      return(as.character(as.integer(round(value))))
    }
    return(formatC(value, digits = digits, format = "fg"))
  }
  as.character(value)
}

.adaptive_progress_value_with_note <- function(x, note = NULL) {
  if (is.null(note)) {
    return(.adaptive_progress_value(x))
  }
  if (is.null(x) || length(x) == 0L) {
    return(paste0("NA (", note, ")"))
  }
  value <- x[[1L]]
  if (is.na(value)) {
    return(paste0("NA (", note, ")"))
  }
  .adaptive_progress_value(value)
}

.adaptive_progress_effective_cores <- function(physical, logical) {
  physical <- suppressWarnings(as.integer(physical %||% NA_integer_))
  logical <- suppressWarnings(as.integer(logical %||% NA_integer_))
  if (!is.na(physical) && physical >= 1L) {
    return(physical)
  }
  if (!is.na(logical) && logical >= 1L) {
    return(logical)
  }
  1L
}

.adaptive_progress_should_iter <- function(config, iter) {
  if (!isTRUE(config$progress)) return(FALSE)
  every <- as.integer(config$progress_every_iter %||% 1L)
  iter <- as.integer(iter)
  if (is.na(every) || every < 1L || is.na(iter)) return(FALSE)
  iter %% every == 0L
}

.adaptive_progress_should_refit <- function(config, round_id) {
  if (!isTRUE(config$progress)) return(FALSE)
  every <- as.integer(config$progress_every_refit %||% 1L)
  round_id <- as.integer(round_id)
  if (is.na(every) || every < 1L || is.na(round_id)) return(FALSE)
  round_id %% every == 0L
}

.adaptive_progress_format_iter_line <- function(batch_row) {
  if (!is.data.frame(batch_row)) {
    rlang::abort("`batch_row` must be a data frame.")
  }
  row <- tibble::as_tibble(batch_row)[1, , drop = FALSE]
  progress_field <- function(name, fallback = NA) {
    if (name %in% names(row)) {
      return(row[[name]][[1L]] %||% fallback)
    }
    fallback
  }
  phase <- progress_field("phase", NA_character_)
  iter <- .adaptive_progress_value(progress_field("iter", NA_integer_))
  n_selected <- .adaptive_progress_value(progress_field("n_pairs_selected", NA_integer_))
  batch_target <- .adaptive_progress_value(progress_field("batch_size_target", NA_integer_))
  n_completed <- .adaptive_progress_value(progress_field("n_pairs_completed", NA_integer_))
  line <- paste0(
    "[", phase, " iter=", iter, "] ",
    "selected=", n_selected, "/", batch_target,
    " completed=", n_completed
  )

  candidate_starved <- progress_field("candidate_starved", NA)
  if (!is.na(candidate_starved)) {
    line <- paste0(line, " starved=", .adaptive_progress_value(candidate_starved))
  }

  fallback_used <- progress_field("fallback_used", NA_character_)
  if (!is.na(fallback_used) && nzchar(fallback_used) &&
    !identical(fallback_used, "base_window")) {
    line <- paste0(line, " fallback=", as.character(fallback_used))
  }

  reason_short_batch <- progress_field("reason_short_batch", NA_character_)
  n_selected_num <- suppressWarnings(as.numeric(progress_field("n_pairs_selected", NA_real_)))
  batch_target_num <- suppressWarnings(as.numeric(progress_field("batch_size_target", NA_real_)))
  if (!is.na(n_selected_num) && !is.na(batch_target_num) &&
    n_selected_num < batch_target_num &&
    !is.na(reason_short_batch) &&
    nzchar(reason_short_batch)) {
    line <- paste0(line, " reason=", as.character(reason_short_batch))
  }
  line
}

.adaptive_progress_format_refit_block <- function(round_row, config, state = NULL) {
  if (!is.data.frame(round_row)) {
    rlang::abort("`round_row` must be a data frame.")
  }
  config <- config %||% list()
  row <- tibble::as_tibble(round_row)[1, , drop = FALSE]
  progress_field <- function(name, fallback = NA) {
    if (name %in% names(row)) {
      return(row[[name]][[1L]] %||% fallback)
    }
    fallback
  }
  rho_theta_lag_val <- progress_field("rho_theta_lag", NA_real_)
  delta_sd_theta_lag_val <- progress_field("delta_sd_theta_lag", NA_real_)
  rho_rank_lag_val <- progress_field("rho_rank_lag", NA_real_)
  lag_eligible_val <- progress_field("lag_eligible", NA)
  batch_index <- NA_integer_
  phase_val <- NA_character_
  if (!is.null(state)) {
    phase_val <- state$phase %||% phase_val
    batch_index <- state$iter %||% batch_index
    batch_log <- state$batch_log %||% tibble::tibble()
    if (is.data.frame(batch_log) && nrow(batch_log) > 0L) {
      phase_val <- phase_val %||% batch_log$phase[[nrow(batch_log)]]
      batch_index <- batch_index %||% batch_log$iter[[nrow(batch_log)]]
    }
  }

  header <- paste0(
    "[adaptive pairing] REFIT ", .adaptive_progress_value(row$round_id),
    "   iter=", .adaptive_progress_value(progress_field("iter_at_refit", NA_integer_)),
    "   phase=", .adaptive_progress_value(phase_val)
  )
  header_sub <- paste0(
    "batch=", .adaptive_progress_value(batch_index),
    "   pairs=", .adaptive_progress_value(progress_field("total_pairs", NA_integer_)),
    " (+", .adaptive_progress_value(progress_field("new_pairs", NA_integer_)), ")"
  )

  reliability_EAP <- progress_field("reliability_EAP", NA_real_)
  reliability_min <- config$eap_reliability_min %||% NA_real_
  eap_pass <- progress_field("eap_pass", NA)
  if (!isTRUE(eap_pass) && !isFALSE(eap_pass)) {
    if (is.finite(reliability_EAP) && is.finite(reliability_min)) {
      eap_pass <- reliability_EAP >= reliability_min
    } else {
      eap_pass <- NA
    }
  }

  diagnostics_pass <- progress_field("diagnostics_pass", NA)
  divergences <- progress_field("divergences", NA_integer_)
  max_rhat <- progress_field("max_rhat", NA_real_)
  min_ess_bulk <- progress_field("min_ess_bulk", NA_real_)

  theta_corr_pass_val <- if (isTRUE(lag_eligible_val)) {
    progress_field("theta_corr_pass", NA)
  } else {
    NA
  }
  delta_sd_theta_pass_val <- if (isTRUE(lag_eligible_val)) {
    progress_field("delta_sd_theta_pass", NA)
  } else {
    NA
  }
  rho_rank_pass_val <- if (isTRUE(lag_eligible_val)) {
    progress_field("rho_rank_pass", NA)
  } else {
    NA
  }

  rho_theta_lag_val <- if (isTRUE(lag_eligible_val)) rho_theta_lag_val else NA_real_
  delta_sd_theta_lag_val <- if (isTRUE(lag_eligible_val)) delta_sd_theta_lag_val else NA_real_
  rho_rank_lag_val <- if (isTRUE(lag_eligible_val)) rho_rank_lag_val else NA_real_

  lines <- c(
    header,
    header_sub,
    "GATES",
    paste0(
      "  diagnostics_pass : ", .adaptive_progress_value(diagnostics_pass),
      "    div=", .adaptive_progress_value(divergences),
      "   max_rhat=", .adaptive_progress_value(max_rhat),
      "   min_ess_bulk=", .adaptive_progress_value(min_ess_bulk)
    ),
    paste0(
      "  eap_pass         : ", .adaptive_progress_value(eap_pass),
      "   reliability_EAP=", .adaptive_progress_value(reliability_EAP),
      "   min=", .adaptive_progress_value(reliability_min)
    ),
    paste0(
      "LAG (eligible=", .adaptive_progress_value(lag_eligible_val),
      ", L=", .adaptive_progress_value(config$stability_lag %||% NA_integer_),
      ")"
    ),
    paste0(
      "  theta_corr_pass     : ", .adaptive_progress_value(theta_corr_pass_val),
      "    rho_theta=", .adaptive_progress_value(rho_theta_lag_val),
      "   min=", .adaptive_progress_value(config$theta_corr_min %||% NA_real_)
    ),
    paste0(
      "  delta_sd_theta_pass : ", .adaptive_progress_value(delta_sd_theta_pass_val),
      "    delta_sd_theta=", .adaptive_progress_value(delta_sd_theta_lag_val),
      "   max=", .adaptive_progress_value(config$theta_sd_rel_change_max %||% NA_real_)
    ),
    paste0(
      "  rho_rank_pass       : ", .adaptive_progress_value(rho_rank_pass_val),
      "    rho_rank=", .adaptive_progress_value(rho_rank_lag_val),
      "   min=", .adaptive_progress_value(config$rank_spearman_min %||% NA_real_)
    ),
    "STOP",
    paste0(
      "  stop_decision : ", .adaptive_progress_value(progress_field("stop_decision", NA))
    )
  )

  stop_decision <- progress_field("stop_decision", NA)
  stop_reason <- progress_field("stop_reason", NA_character_)
  if (isTRUE(stop_decision %in% TRUE) &&
    (is.na(stop_reason) || identical(stop_reason, "v3_converged"))) {
    stop_reason <- "all_gates_passed"
  }
  lines <- c(
    lines,
    paste0(
      "  stop_reason   : ", .adaptive_progress_value(stop_reason)
    )
  )
  lines
}

.adaptive_progress_emit_iter <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  config <- state$config$v3 %||% list()
  if (!isTRUE(config$progress)) return(invisible(FALSE))
  batch_log <- state$batch_log %||% tibble::tibble()
  if (!is.data.frame(batch_log) || nrow(batch_log) == 0L) {
    return(invisible(FALSE))
  }
  batch_row <- batch_log[nrow(batch_log), , drop = FALSE]
  if (!.adaptive_progress_should_iter(config, batch_row$iter %||% NA_integer_)) {
    return(invisible(FALSE))
  }
  line <- .adaptive_progress_format_iter_line(batch_row)
  cat(line, "\n", sep = "")
  invisible(TRUE)
}

.adaptive_progress_emit_refit <- function(state, round_row, config = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  config <- config %||% state$config$v3 %||% list()
  if (!isTRUE(config$progress)) return(invisible(FALSE))
  if (identical(.adaptive_progress_level(config), "basic")) return(invisible(FALSE))
  round_id <- round_row$round_id %||% NA_integer_
  if (!.adaptive_progress_should_refit(config, round_id)) {
    return(invisible(FALSE))
  }
  lines <- .adaptive_progress_format_refit_block(round_row, config, state = state)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(TRUE)
}
