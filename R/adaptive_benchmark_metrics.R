# -------------------------------------------------------------------------
# Benchmark metric extractors (report-only).
# -------------------------------------------------------------------------

.adaptive_link_mix_metrics <- function(step_log) {
  log <- tibble::as_tibble(step_log)
  if (nrow(log) == 0L) {
    return(tibble::tibble(
      round_id = integer(),
      round_stage = character(),
      pair_type = character(),
      committed = integer(),
      proportion = double()
    ))
  }

  staged <- log[
    log$round_stage %in% .adaptive_stage_order() &
      !is.na(log$pair_id) &
      !is.na(log$round_id),
    ,
    drop = FALSE
  ]
  if (nrow(staged) == 0L) {
    return(tibble::tibble(
      round_id = integer(),
      round_stage = character(),
      pair_type = character(),
      committed = integer(),
      proportion = double()
    ))
  }

  counts <- staged |>
    dplyr::count(.data$round_id, .data$round_stage, .data$pair_type, name = "committed")
  totals <- staged |>
    dplyr::count(.data$round_id, .data$round_stage, name = "stage_total")
  out <- counts |>
    dplyr::left_join(totals, by = c("round_id", "round_stage")) |>
    dplyr::mutate(proportion = as.double(.data$committed / .data$stage_total)) |>
    dplyr::select("round_id", "round_stage", "pair_type", "committed", "proportion")

  tibble::as_tibble(out)
}

.adaptive_fallback_distribution <- function(step_log) {
  log <- tibble::as_tibble(step_log)
  if (nrow(log) == 0L) {
    return(tibble::tibble(
      fallback_used = character(),
      attempts = integer(),
      share = double()
    ))
  }
  counts <- log |>
    dplyr::count(.data$fallback_used, name = "attempts")
  denom <- sum(counts$attempts)
  counts$share <- as.double(counts$attempts / denom)
  tibble::as_tibble(counts)
}

.adaptive_time_call <- function(fn) {
  start <- proc.time()[["elapsed"]]
  value <- fn()
  elapsed <- as.double(proc.time()[["elapsed"]] - start)
  list(
    value = value,
    elapsed = max(0, elapsed)
  )
}

.adaptive_efficiency_profile_empty <- function() {
  list(
    current_phase = NA_character_,
    warm_start_active = NA,
    latest_step_status = NA_character_,
    latest_step_round_stage = NA_character_,
    latest_step_candidate_starved = NA,
    latest_step_starvation_reason = NA_character_,
    predicted_selection_candidate_starved = NA,
    predicted_selection_round_stage = NA_character_,
    refit_profile_state_source = "current_state",
    timings = c(
      select_next_pair = NA_real_,
      round_starvation = NA_real_,
      maybe_refit_btl = NA_real_,
      phase_a_prepare = NA_real_,
      phase_a_finalize_if_ready = NA_real_
    )
  )
}

.adaptive_efficiency_profile <- function(state, config = NULL, fit_fn = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }

  out <- .adaptive_efficiency_profile_empty()
  controller <- .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  out$current_phase <- as.character(phase_ctx$phase %||% "phase_a")
  out$warm_start_active <- isTRUE(.adaptive_warm_start_active(state))

  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  latest_step <- if (nrow(step_log) > 0L) {
    step_log[nrow(step_log), , drop = FALSE]
  } else {
    tibble::tibble()
  }
  if (nrow(latest_step) > 0L) {
    out$latest_step_status <- if ("status" %in% names(latest_step)) {
      as.character(latest_step$status[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }
    out$latest_step_round_stage <- if ("round_stage" %in% names(latest_step)) {
      as.character(latest_step$round_stage[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }
    out$latest_step_candidate_starved <- if ("candidate_starved" %in% names(latest_step)) {
      isTRUE(latest_step$candidate_starved[[1L]] %||% FALSE)
    } else {
      FALSE
    }
    out$latest_step_starvation_reason <- as.character(
      if ("starvation_reason" %in% names(latest_step)) {
        latest_step$starvation_reason[[1L]] %||% NA_character_
      } else {
        NA_character_
      }
    )
  }

  prepare_timed <- .adaptive_time_call(function() .adaptive_phase_a_prepare(state))
  out$timings[["phase_a_prepare"]] <- as.double(prepare_timed$elapsed)
  finalize_timed <- .adaptive_time_call(function() {
    .adaptive_phase_a_finalize_if_ready(prepare_timed$value)
  })
  out$timings[["phase_a_finalize_if_ready"]] <- as.double(finalize_timed$elapsed)

  if (!isTRUE(.adaptive_warm_start_active(state))) {
    select_timed <- .adaptive_time_call(function() {
      select_next_pair(state, step_id = as.integer(nrow(step_log) + 1L))
    })
    out$timings[["select_next_pair"]] <- as.double(select_timed$elapsed)
    selection <- select_timed$value %||% list()
    out$predicted_selection_candidate_starved <- isTRUE(selection$candidate_starved %||% FALSE)
    out$predicted_selection_round_stage <- as.character(selection$round_stage %||% NA_character_)
  }

  refit_state <- state
  if (isTRUE(out$latest_step_candidate_starved) &&
    !identical(out$latest_step_round_stage, "warm_start") &&
    nrow(latest_step) > 0L) {
    starvation_timed <- .adaptive_time_call(function() {
      .adaptive_round_starvation(state, latest_step)
    })
    out$timings[["round_starvation"]] <- as.double(starvation_timed$elapsed)
    starve_value <- starvation_timed$value %||% list()
    refit_state <- starve_value$state %||% state
    out$refit_profile_state_source <- "post_starvation_replay"
  }

  refit_timed <- .adaptive_time_call(function() {
    maybe_refit_btl(refit_state, config = config %||% list(), fit_fn = fit_fn)
  })
  out$timings[["maybe_refit_btl"]] <- as.double(refit_timed$elapsed)

  out
}

.adaptive_efficiency_context_rows <- function(profile) {
  rows <- list(
    tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = NA_character_,
      metric = paste0("phase:", as.character(profile$current_phase %||% "unknown")),
      value = 1,
      report_only = TRUE
    ),
    tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = NA_character_,
      metric = "warm_start_active",
      value = as.double(isTRUE(profile$warm_start_active)),
      report_only = TRUE
    ),
    tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = NA_character_,
      metric = "latest_step_candidate_starved",
      value = as.double(isTRUE(profile$latest_step_candidate_starved)),
      report_only = TRUE
    ),
    tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = NA_character_,
      metric = "predicted_selection_candidate_starved",
      value = as.double(isTRUE(profile$predicted_selection_candidate_starved)),
      report_only = TRUE
    ),
    tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = NA_character_,
      metric = paste0(
        "refit_profile_state_source:",
        as.character(profile$refit_profile_state_source %||% "current_state")
      ),
      value = 1,
      report_only = TRUE
    )
  )

  if (!is.na(profile$latest_step_status %||% NA_character_)) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = NA_character_,
      metric = paste0("latest_step_status:", as.character(profile$latest_step_status)),
      value = 1,
      report_only = TRUE
    )
  }
  if (!is.na(profile$latest_step_round_stage %||% NA_character_)) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = as.character(profile$latest_step_round_stage),
      metric = paste0("latest_step_round_stage:", as.character(profile$latest_step_round_stage)),
      value = 1,
      report_only = TRUE
    )
  }
  if (!is.na(profile$latest_step_starvation_reason %||% NA_character_)) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = NA_character_,
      metric = paste0(
        "latest_step_starvation_reason:",
        as.character(profile$latest_step_starvation_reason)
      ),
      value = 1,
      report_only = TRUE
    )
  }
  if (!is.na(profile$predicted_selection_round_stage %||% NA_character_)) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      metric_group = "efficiency_context",
      round_id = NA_integer_,
      round_stage = as.character(profile$predicted_selection_round_stage),
      metric = paste0(
        "predicted_selection_round_stage:",
        as.character(profile$predicted_selection_round_stage)
      ),
      value = 1,
      report_only = TRUE
    )
  }

  dplyr::bind_rows(rows)
}

.adaptive_efficiency_timing_rows <- function(profile) {
  timings <- as.double(profile$timings %||% numeric())
  timing_names <- names(profile$timings %||% numeric())
  if (length(timings) < 1L || is.null(timing_names)) {
    return(tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    ))
  }

  tibble::tibble(
    metric_group = "efficiency_timing",
    round_id = NA_integer_,
    round_stage = NA_character_,
    metric = paste0("elapsed_seconds:", as.character(timing_names)),
    value = as.double(timings),
    report_only = TRUE
  )
}

.adaptive_benchmark_metrics <- function(state, include_efficiency_profile = FALSE, config = NULL, fit_fn = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }

  step_log <- adaptive_step_log(state)
  round_log <- adaptive_round_log(state)
  link_mix <- .adaptive_link_mix_metrics(step_log)
  fallback <- .adaptive_fallback_distribution(step_log)
  quota <- .adaptive_stage_quota_summary(step_log)

  top_level <- tibble::tibble(
    metric_group = "run",
    round_id = NA_integer_,
    round_stage = NA_character_,
    metric = c("steps_attempted", "steps_committed", "refit_rows"),
    value = c(
      as.double(nrow(step_log)),
      as.double(sum(!is.na(step_log$pair_id))),
      as.double(nrow(round_log))
    ),
    report_only = TRUE
  )

  link_rows <- if (nrow(link_mix) == 0L) {
    tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    )
  } else {
    link_mix |>
      dplyr::transmute(
        metric_group = "link_mix",
        round_id = as.integer(.data$round_id),
        round_stage = as.character(.data$round_stage),
        metric = paste0("pair_type:", .data$pair_type, ":proportion"),
        value = as.double(.data$proportion),
        report_only = TRUE
      )
  }

  fallback_rows <- if (nrow(fallback) == 0L) {
    tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    )
  } else {
    fallback |>
      dplyr::transmute(
        metric_group = "fallback",
        round_id = NA_integer_,
        round_stage = NA_character_,
        metric = paste0("fallback:", .data$fallback_used, ":share"),
        value = as.double(.data$share),
        report_only = TRUE
      )
  }

  quota_rows <- if (nrow(quota) == 0L) {
    tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    )
  } else {
    quota |>
      dplyr::transmute(
        metric_group = "quota",
        round_id = as.integer(.data$round_id),
        round_stage = as.character(.data$round_stage),
        metric = "stage_shortfall",
        value = as.double(.data$shortfall),
        report_only = TRUE
      )
  }

  efficiency_rows <- if (isTRUE(include_efficiency_profile)) {
    profile <- .adaptive_efficiency_profile(
      state = state,
      config = config,
      fit_fn = fit_fn
    )
    dplyr::bind_rows(
      .adaptive_efficiency_context_rows(profile),
      .adaptive_efficiency_timing_rows(profile)
    )
  } else {
    tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    )
  }

  dplyr::bind_rows(top_level, link_rows, fallback_rows, quota_rows, efficiency_rows)
}
