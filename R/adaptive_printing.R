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

.adaptive_progress_format_refit_block <- function(round_row, config) {
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
  theta_sd_val <- progress_field("theta_sd_eap", NA_real_)
  rho_theta_lag_val <- progress_field("rho_theta_lag", NA_real_)
  delta_sd_theta_lag_val <- progress_field("delta_sd_theta_lag", NA_real_)
  rho_rank_lag_val <- progress_field("rho_rank_lag", NA_real_)
  rank_stability_pass_val <- progress_field("rank_stability_pass", NA)
  hard_cap_reached_val <- progress_field("hard_cap_reached", NA)
  header <- paste0(
    "[REFIT r=", .adaptive_progress_value(row$round_id),
    " iter=", .adaptive_progress_value(row$iter_at_refit),
    " mode=", .adaptive_progress_value(progress_field("mode", NA_character_)), "]"
  )

  reliability_EAP <- progress_field("reliability_EAP", NA_real_)
  reliability_min <- config$eap_reliability_min %||% NA_real_
  reliability_pass <- NA
  if (is.finite(reliability_EAP) && is.finite(reliability_min)) {
    reliability_pass <- reliability_EAP >= reliability_min
  }

  lines <- c(
    header,
    paste0(
      "  Pairs: completed=", .adaptive_progress_value(progress_field("completed_pairs", NA_integer_)),
      "/", .adaptive_progress_value(progress_field("scheduled_pairs", NA_integer_)),
      " backlog=", .adaptive_progress_value(progress_field("backlog_unjudged", NA_integer_))
    ),
    {
      cores_physical <- progress_field("mcmc_cores_detected_physical", NA_integer_)
      cores_logical <- progress_field("mcmc_cores_detected_logical", NA_integer_)
      cores_effective <- .adaptive_progress_effective_cores(cores_physical, cores_logical)
      mcmc_line <- paste0(
        "  MCMC config: chains=", .adaptive_progress_value(progress_field("mcmc_chains", NA_integer_)),
        " parallel=", .adaptive_progress_value(progress_field("mcmc_parallel_chains", NA_integer_)),
        " cores=", .adaptive_progress_value(cores_physical),
        "/", .adaptive_progress_value(cores_logical),
        " eff=", .adaptive_progress_value(cores_effective)
      )
      core_fraction <- progress_field("mcmc_core_fraction", NA_real_)
      if (!is.na(core_fraction)) {
        mcmc_line <- paste0(
          mcmc_line,
          " core_fraction=",
          .adaptive_progress_value(core_fraction)
        )
      }
      mcmc_line
    },
    paste0(
      "  MCMC: div=", .adaptive_progress_value(progress_field("divergences", NA_integer_)),
      " rhat_max=", .adaptive_progress_value(progress_field("max_rhat", NA_real_)),
      " ess_min=", .adaptive_progress_value(progress_field("min_ess_bulk", NA_real_))
    ),
    paste0(
      "  Diagnostics: pass=", .adaptive_progress_value(progress_field("diagnostics_pass", NA))
    ),
    paste0(
      "  Reliability: EAP=", .adaptive_progress_value(reliability_EAP),
      " pass=", .adaptive_progress_value(reliability_pass)
    ),
    paste0(
      "  Theta: sd_eap=", .adaptive_progress_value(theta_sd_val)
    )
  )

  lines <- c(
    lines,
    paste0(
      "  Stability: rho_theta=",
      .adaptive_progress_value_with_note(rho_theta_lag_val, "not eligible yet"),
      " delta_sd=",
      .adaptive_progress_value_with_note(delta_sd_theta_lag_val, "not eligible yet"),
      " rho_rank=",
      .adaptive_progress_value_with_note(rho_rank_lag_val, "not eligible yet"),
      " rank_pass=",
      .adaptive_progress_value_with_note(rank_stability_pass_val, "not eligible yet")
    )
  )

  lines <- c(
    lines,
    paste0(
      "  Stop: eligible=",
      .adaptive_progress_value(progress_field("stop_eligible", NA)),
      " decision=",
      .adaptive_progress_value(progress_field("stop_decision", NA))
    )
  )
  stop_decision <- progress_field("stop_decision", NA)
  if (isTRUE(stop_decision %in% TRUE)) {
    lines <- c(
      lines,
      paste0(
        "  Stop reason: ",
        .adaptive_progress_value(progress_field("stop_reason", NA_character_))
      )
    )
  }

  if (identical(.adaptive_progress_level(config), "full")) {
    lines <- c(
      lines,
      paste0(
        "  Hard cap: seen=", .adaptive_progress_value(row$n_unique_pairs_seen),
        " cap=", .adaptive_progress_value(row$hard_cap_threshold),
        " reached=", .adaptive_progress_value(hard_cap_reached_val)
      )
    )
  }
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
  lines <- .adaptive_progress_format_refit_block(round_row, config)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(TRUE)
}
