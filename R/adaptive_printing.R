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
  phase <- row$phase %||% NA_character_
  iter <- .adaptive_progress_value(row$iter)
  n_selected <- .adaptive_progress_value(row$n_pairs_selected)
  batch_target <- .adaptive_progress_value(row$batch_size_target)
  n_completed <- .adaptive_progress_value(row$n_pairs_completed)
  line <- paste0(
    "[", phase, " iter=", iter, "] ",
    "selected=", n_selected, "/", batch_target,
    " completed=", n_completed
  )

  candidate_starved <- row$candidate_starved %||% NA
  if (!is.na(candidate_starved)) {
    line <- paste0(line, " starved=", .adaptive_progress_value(candidate_starved))
  }

  reason_short_batch <- row$reason_short_batch %||% NA_character_
  n_selected_num <- suppressWarnings(as.numeric(row$n_pairs_selected))
  batch_target_num <- suppressWarnings(as.numeric(row$batch_size_target))
  if (!is.na(n_selected_num) && !is.na(batch_target_num) &&
    n_selected_num < batch_target_num &&
    !is.na(reason_short_batch) &&
    nzchar(reason_short_batch)) {
    line <- paste0(line, " reason=", as.character(reason_short_batch))
  }
  line
}

.adaptive_progress_format_refit_block <- function(round_row, state, config) {
  if (!is.data.frame(round_row)) {
    rlang::abort("`round_row` must be a data frame.")
  }
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  config <- config %||% state$config$v3 %||% list()
  row <- tibble::as_tibble(round_row)[1, , drop = FALSE]
  phase <- state$phase %||% NA_character_
  header <- paste0(
    "[REFIT r=", .adaptive_progress_value(row$round_id),
    " iter=", .adaptive_progress_value(row$iter_at_refit),
    " ", phase, "]"
  )

  lines <- c(
    header,
    {
      cores_physical <- row$mcmc_cores_detected_physical %||% NA_integer_
      cores_logical <- row$mcmc_cores_detected_logical %||% NA_integer_
      cores_effective <- .adaptive_progress_effective_cores(cores_physical, cores_logical)
      mcmc_line <- paste0(
        "  MCMC config: chains=", .adaptive_progress_value(row$mcmc_chains),
        " parallel=", .adaptive_progress_value(row$mcmc_parallel_chains),
        " cores=", .adaptive_progress_value(cores_physical),
        "/", .adaptive_progress_value(cores_logical),
        " eff=", .adaptive_progress_value(cores_effective)
      )
      core_fraction <- row$mcmc_core_fraction %||% NA_real_
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
      "  MCMC: div=", .adaptive_progress_value(row$divergences),
      " rhat_max=", .adaptive_progress_value(row$max_rhat),
      " ess_min=", .adaptive_progress_value(row$min_ess_bulk)
    ),
    paste0(
      "  eps_mean=", .adaptive_progress_value(row$epsilon_mean),
      " rel_EAP=", .adaptive_progress_value(row$reliability_EAP)
    ),
    paste0(
      "  Gate: diagnostics_pass=", .adaptive_progress_value(row$diagnostics_pass)
    ),
    paste0(
      "  SD: median_S=", .adaptive_progress_value(row$theta_sd_median),
      " tau=", .adaptive_progress_value(row$tau),
      " pass=", .adaptive_progress_value(row$theta_sd_pass)
    ),
    paste0(
      "  U: U0=", .adaptive_progress_value(row$U0),
      " U_abs=", .adaptive_progress_value(row$U_abs),
      " pass=", .adaptive_progress_value(row$U_pass)
    )
  )

  has_stability <- !(is.na(row$frac_weak_adj) &&
    is.na(row$min_adj_prob) &&
    is.na(row$rank_stability_pass))
  if (isTRUE(has_stability)) {
    lines <- c(
      lines,
      paste0(
        "  Stability: weak=", .adaptive_progress_value(row$frac_weak_adj),
        " min_adj=", .adaptive_progress_value(row$min_adj_prob),
        " pass=", .adaptive_progress_value(row$rank_stability_pass)
      )
    )
  }

  checks_passed <- state$checks_passed_in_row %||% NA_integer_
  checks_target <- config$checks_passed_target %||% NA_integer_
  if (!is.na(checks_passed) || !is.na(checks_target)) {
    lines <- c(
      lines,
      paste0(
        "  Stop streak: ",
        .adaptive_progress_value(checks_passed), "/",
        .adaptive_progress_value(checks_target)
      )
    )
  }

  if (identical(.adaptive_progress_level(config), "full")) {
    lines <- c(
      lines,
      paste0(
        "  Hard cap: seen=", .adaptive_progress_value(row$n_unique_pairs_seen),
        " cap=", .adaptive_progress_value(row$hard_cap_threshold),
        " reached=", .adaptive_progress_value(row$hard_cap_reached)
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
  lines <- .adaptive_progress_format_refit_block(round_row, state, config)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(TRUE)
}
